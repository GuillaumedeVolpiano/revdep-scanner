{-# LANGUAGE LambdaCase #-}

module RevdepScanner
  ( prettyProblems
  , prettyMatches
  , prettyResults
  , ConstraintMap
  , MatchMode(Matching, NonMatching)
  , Debug
  , Mode(HelpMode, NormalMode)
  , lookupResults
  , args
  , runOpaque
  , runTransparent
  , parseAll
  ) where

import           Conduit                    (ConduitT, decodeUtf8LenientC,
                                             iterMC, sinkLazy, (.|))
import           Control.Monad.Trans.Accum  (AccumT, add, evalAccumT, look)
import qualified Data.ByteString            as BS
import           Data.Conduit.Process       (sourceProcessWithStreams)
import           Data.Function              (on)
import           Data.HashMap.Strict        (HashMap)
import qualified Data.HashMap.Strict        as M
import           Data.HashSet               (HashSet)
import qualified Data.HashSet               as S
import           Data.List                  as L
import           Data.List.NonEmpty         (NonEmpty (..))
import qualified Data.List.NonEmpty         as NE
import           Data.Monoid                (Any, Last, Sum (Sum))
import           Data.Parsable              (ParseError, choice, eof, lift,
                                             parse, parser, toString, try)
import qualified Data.Text.Lazy             as TL
import           Data.Void
import           Distribution.Portage.Types
import           System.Exit                (ExitCode)
import           System.IO                  (Handle, stderr, stdout)
import           System.Process             (delegate_ctlc, proc)
import           Text.Parsec.Char           (anyChar)
import           Text.Parsec.String         (Parser)

lookupResults ::
     MatchMode -> Package -> ConstraintMap -> HashMap Revdep (HashSet ParsedDep)
lookupResults mode p0@(Package c0 n0 _ _ _) m0 =
  case M.lookup (c0, n0) m0 of
    Just m  -> foldr (M.unionWith S.union . go) M.empty (M.toList m)
    Nothing -> M.empty
  where
    go :: (Revdep, HashSet ParsedDep) -> HashMap Revdep (HashSet ParsedDep)
    go (r, s)
      | any check s = M.singleton r s
      | otherwise = M.empty
    check d =
      case (mode, d) of
        (Matching, Left cd)      -> doesConstraintMatch cd p0
        (Matching, Right (c, n)) -> c == c0 && n == n0
        (NonMatching, Left cd)   -> not (doesConstraintMatch cd p0)
        (NonMatching, Right _)   -> False

-- Command line
type Debug = Any

data MatchMode
  = Matching
  | NonMatching
  deriving (Show, Eq, Ord)

data Mode
  = HelpMode
  | NormalMode (Last MatchMode) (Last Repository) Debug
  deriving (Show, Eq, Ord)

instance Semigroup Mode where
  HelpMode <> _ = HelpMode
  _ <> HelpMode = HelpMode
  NormalMode m1 r1 d1 <> NormalMode m2 r2 d2 =
    NormalMode (m1 <> m2) (r1 <> r2) (d1 <> d2)

instance Monoid Mode where
  mempty = NormalMode mempty mempty mempty

-- Types
type ConstraintPkg = (Category, PkgName)

type Revdep = (Category, PkgName, Version, Slot, Repository)

type BasicDep = (Category, PkgName)

type ParsedDep = Either ConstrainedDep BasicDep

-- | Organized by @(Category, PkgName)@
--
--   The inner map is keyed by the reverse dependency and contains a set of
--   constraints that match the same @(Category, PkgName)@ as the outermost key.
type ConstraintMap = HashMap ConstraintPkg (HashMap Revdep (HashSet ParsedDep))

insertCM :: Revdep -> ParsedDep -> ConstraintMap -> ConstraintMap
insertCM revdep dep cmap0 =
  unionCM cmap0
    $ M.singleton (ccat, cpkg) (M.singleton revdep (S.singleton dep))
  where
    (ccat, cpkg) =
      case dep of
        Left (ConstrainedDep _ c p _ _ _) -> (c, p)
        Right bDep                        -> bDep

unionCM :: ConstraintMap -> ConstraintMap -> ConstraintMap
unionCM = M.unionWith (M.unionWith S.union)

prettyProblems ::
     Package -> HashMap Revdep (HashSet ParsedDep) -> NonEmpty String
prettyProblems p m
  | M.null m = NE.singleton $ toString p ++ ": No problematic packages found!"
  | otherwise = (toString p ++ ":") :| prettyResults m

prettyMatches ::
     Package -> HashMap Revdep (HashSet ParsedDep) -> NonEmpty String
prettyMatches p m
  | M.null m = NE.singleton $ toString p ++ ": No matches found!"
  | otherwise = (toString p ++ ":") :| prettyResults m

prettyResults :: HashMap Revdep (HashSet ParsedDep) -> [String]
prettyResults m =
  sortBy (compare `on` fst) (M.toList m) >>= \((c, n, v, sl, r), s) ->
    let p = Package c n (Just v) (Just sl) (Just r)
        svs = sortBy cmp (S.toList s)
     in ["    " ++ toString p, "        ( " ++ unwords (map toStr svs) ++ " )"]
  where
    cmp :: ParsedDep -> ParsedDep -> Ordering
    cmp pd1 pd2 =
      case (pd1, pd2) of
        (Left (ConstrainedDep _ _ _ v1 _ _), Left (ConstrainedDep _ _ _ v2 _ _)) ->
          v1 `compare` v2
        (Left _, Right _) -> GT
        (Right _, Left _) -> LT
        (_, _) -> EQ
    toStr :: ParsedDep -> String
    toStr =
      \case
        Left cd -> toString cd
        Right (c, n) -> toString $ Package c n Nothing Nothing Nothing

-- Parsing
parseAll :: TL.Text -> Either ParseError ConstraintMap
parseAll t = do
  cms <- evalAccumT (traverse parseLine (TL.lines t)) (Sum 1)
  pure $ foldr unionCM M.empty cms
  where
    parseLine :: TL.Text -> AccumT (Sum Int) (Either ParseError) ConstraintMap
    parseLine l = do
      Sum i <- look
      add (Sum 1)
      lift $ parse lineParser ("line " ++ show i) (TL.unpack l)

lineParser :: Parser ConstraintMap
lineParser = do
  ConstrainedDep Equal rdCat rdPkg rdVer (Just rdSlot) (Just rdRepo) <-
    parser @ConstrainedDep -- parser from Data.Parsable
  let revdep = (rdCat, rdPkg, rdVer, rdSlot, rdRepo)
  foldr (insertCM revdep) M.empty <$> bruteForce
  where
    -- Start with char 0, see if it's a valid ConstrainedDep /or/ Package.
    -- Try next char, see if it's a valid ConstrainedDep /or Package.
    -- etc...
    bruteForce :: Parser [ParsedDep]
    bruteForce =
      choice
        [ try $ (:) <$> (Left <$> parser @ConstrainedDep) <*> bruteForce
        , try $ do
            Package c n Nothing _ _ <- parser
            (Right (c, n) :) <$> bruteForce
        , try $ [] <$ eof
        , anyChar *> bruteForce
        ]

-- Util stuff
-- | Run a command and capture stdout and stderr
runOpaque ::
     FilePath -- ^ executable path
  -> [String] -- ^ arguments
       -- | Exit code, stdout, stderr
  -> IO (ExitCode, TL.Text, TL.Text)
runOpaque exe a =
  sourceProcessWithStreams
    (proc exe a)
    (pure ())
    (decodeUtf8LenientC .| sinkLazy)
    (decodeUtf8LenientC .| sinkLazy)

-- | Run a command and dump stdout to @stdout@, stderr to @stderr@, also
--   capturing both streams.
runTransparent ::
     FilePath -- ^ executable path
  -> [String] -- ^ arguments
       -- | Exit code, stdout, stderr
  -> IO (ExitCode, TL.Text, TL.Text)
runTransparent exe a =
  sourceProcessWithStreams
    (proc exe a) {delegate_ctlc = True}
    (pure ())
    (transSink stdout)
    (transSink stderr)
  where
    transSink :: Handle -> ConduitT BS.ByteString Void IO TL.Text
    transSink h = iterMC (BS.hPut h) .| decodeUtf8LenientC .| sinkLazy

args :: Repository -> [String]
args (Repository n) =
  [ "--all"
  , "--raw"
  , "--unfiltered"
  , "--repo"
  , n
  , "--atom"
  , "--cpv"
  , "--slot"
  , "--attr"
  , "depend"
  , "--attr"
  , "rdepend"
  , "--attr"
  , "bdepend"
  , "-R"
  , "--slot"
  ]
