{-# Language DeriveTraversable #-}
{-# Language DerivingVia #-}
{-# Language LambdaCase #-}
{-# Language TypeApplications #-}

module Main where

import Conduit
import Control.Monad
import Control.Monad.Trans.Accum
import qualified Data.ByteString as BS
import Data.Function (on)
import Data.List as L
import qualified Data.HashMap.Strict as M
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashSet as S
import           Data.HashSet (HashSet)
import Data.Maybe (fromMaybe)
import qualified Data.Text.Lazy as TL
import Data.Conduit.Process
import Data.Monoid
import System.Directory
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO (Handle, stdout, stderr)
import Text.Parsec.Char
import Text.Parsec.String

import Text.Pretty.Simple (pPrintForceColor)

import Data.Parsable
import Distribution.Portage.Types

main :: IO ()
main = do
    pquery <- runEnv pqueryPath
    (p, mode, repoName, Any d) <- checkArgs

    when d $ print $ pquery : args repoName

    let f = if d then runTransparent else runOpaque

    (_, out, _) <- f pquery (args repoName)
    Right (m :: ConstraintMap) <- pure $ parseAll out

    when d $ pPrintForceColor m

    let r = lookupResults mode p m
    putStrLn $ case mode of
        Matching -> prettyMatches r
        NonMatching -> prettyProblems r
  where
    args (Repository n) =
        [ "--all"
        , "--raw"
        , "--unfiltered"
        , "--repo", n
        , "--atom"
        , "--cpv"
        , "--slot"
        , "--attr", "depend"
        , "--attr", "rdepend"
        , "--attr", "bdepend"
        , "-R"
        , "--slot"
        ]

prettyProblems :: HashMap Revdep (HashSet ConstrainedDep) -> String
prettyProblems m
    | M.null m = "No problematic packages found!"
    | otherwise = unlines
        $ "Packages with at least one problematic constraint:"
        : ""
        : "package"
        : "    ( relevant dependencies ):"
        : ""
        : [ prettyResults m ]

prettyMatches :: HashMap Revdep (HashSet ConstrainedDep) -> String
prettyMatches m
    | M.null m = "No matches found!"
    | otherwise = unlines
        $ "Packages with at least one matching dependency:"
        : ""
        : "package"
        : "    ( relevant dependencies ):"
        : ""
        : [ prettyResults m ]

prettyResults :: HashMap Revdep (HashSet ConstrainedDep) -> String
prettyResults m = unlines
        $ sortBy (compare `on` fst) (M.toList m) >>= \((c,n,v),s) ->
                let p = Package c n (Just v) Nothing Nothing
                    svs = sortBy (compare `on` constrainedVersion) (S.toList s)
                in  [ toString p
                    , "    ( " ++ L.intercalate " " (map toString svs) ++ " )"
                    ]

lookupResults
    :: MatchMode
    -> Package
    -> ConstraintMap
    -> HashMap Revdep (HashSet ConstrainedDep)
lookupResults mode p0@(Package c0 n0 _ _ _) m0 =
    case M.lookup (c0, n0) m0 of
        Just m -> foldr (M.unionWith S.union . go) M.empty (M.toList m)
        Nothing -> M.empty
  where
    go :: (Revdep, HashSet ConstrainedDep)
       -> HashMap Revdep (HashSet ConstrainedDep)
    go (r, s)
        | any check s = M.singleton r s
        | otherwise = M.empty

    check d = case mode of
        Matching -> doesConstraintMatch d p0
        NonMatching -> not (doesConstraintMatch d p0)

-- Types

type ConstraintPkg = (Category, PkgName)
type Revdep = (Category, PkgName, Version)

-- | Organized by @(Category, PkgName)@
--
--   The inner map is keyed by the reverse dependency and contains a set of
--   constraints that match the same @(Category, PkgName)@ as the outermost key.
type ConstraintMap = HashMap ConstraintPkg
    (HashMap Revdep (HashSet ConstrainedDep))

insertCM :: Revdep -> ConstrainedDep -> ConstraintMap -> ConstraintMap
insertCM revdep cdep@(ConstrainedDep _ ccat cpkg _ _ _) cmap0 =
    unionCM cmap0 $
        M.singleton (ccat,cpkg) (M.singleton revdep (S.singleton cdep))

unionCM :: ConstraintMap -> ConstraintMap -> ConstraintMap
unionCM = M.unionWith (M.unionWith S.union)

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
    ConstrainedDep Equal rdCat rdPkg rdVer _ _ <-
        parser @ConstrainedDep -- parser from Data.Parsable
    let revdep = (rdCat, rdPkg, rdVer)
    cds <- bruteForce
    pure $ foldr (insertCM revdep) M.empty cds
  where
    -- Start with char 0, see if it's a valid ConstrainedDep
    -- Try next char, see if it's a valid ConstrainedDep
    -- etc...
    bruteForce :: Parser [ConstrainedDep]
    bruteForce = choice
        [ try $ (:) <$> parser @ConstrainedDep <*> bruteForce
        , try $ [] <$ eof
        , anyChar *> bruteForce
        ]

-- Environment stuff

type Env = AccumT (First FilePath) IO

runEnv :: Env a -> IO a
runEnv = flip evalAccumT mempty

-- | Find the path to the @pquery@ executable or throw an error. Caches the
--   result in the case of a success.
pqueryPath :: Env FilePath
pqueryPath = look >>= \case
    First (Just p) -> pure p
    First Nothing -> liftIO (findExecutable "pquery") >>= \case
        Just p -> add (pure p) *> pure p
        Nothing -> liftIO $
            die "Could not find pquery executable. Install sys-apps/pkgcore first."

-- Util stuff

-- | Run a command and capture stdout and stderr
runOpaque
    :: FilePath -- ^ executable path
    -> [String] -- ^ arguments
       -- | Exit code, stdout, stderr
    -> IO (ExitCode, TL.Text, TL.Text)
runOpaque exe args
    = sourceProcessWithStreams
        (proc exe args)
        (pure ())
        (decodeUtf8LenientC .| sinkLazy)
        (decodeUtf8LenientC .| sinkLazy)

-- | Run a command and dump stdout to @stdout@, stderr to @stderr@, also
--   capturing both streams.
runTransparent
    :: FilePath -- ^ executable path
    -> [String] -- ^ arguments
       -- | Exit code, stdout, stderr
    -> IO (ExitCode, TL.Text, TL.Text)
runTransparent exe args
    = sourceProcessWithStreams (proc exe args) { delegate_ctlc = True }
            (pure ()) (transSink stdout) (transSink stderr)
  where
    transSink :: Handle -> ConduitT BS.ByteString Void IO TL.Text
    transSink h = iterMC (BS.hPut h) .| decodeUtf8LenientC .| sinkLazy

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
    NormalMode m1 r1 d1 <> NormalMode m2 r2 d2
        = NormalMode (m1 <> m2) (r1 <> r2) (d1 <> d2)

instance Monoid Mode where
    mempty = NormalMode mempty mempty mempty

checkArgs :: IO (Package, MatchMode, Repository, Debug)
checkArgs = do
    progName <- getProgName
    argv <- getArgs
    let err str = showHelp progName *> die ("error: " ++ str)

    case getOpt Permute options argv of
        (_,_,es@(_:_)) -> err (intercalate " " es)

        (ms,as,_) -> case (mconcat ms, as) of
            (HelpMode, _) -> showHelp progName *> exitSuccess
            (_, []) -> err "Full package name and version required"
            (_,(_:as'@(_:_))) -> err
                ("Extra command-line arguments given: " ++ show as')
            (NormalMode (Last mm) (Last mr) d, [pStr]) ->
                case runParsable "command line argument" pStr of
                    Left e -> err $
                        "Invalid package: " ++ show e
                    Right p@(Package _ _ mv _ _) ->
                        let m = case (mm, mv) of
                                    (Just m', _) -> m'
                                    (Nothing, Just _) -> NonMatching
                                    _ -> Matching
                        in  pure (p, m, fromMaybe (Repository "haskell") mr, d)
  where
    showHelp progName = putStrLn (usageInfo (header progName) options)

    header progName = unlines $ unwords <$>
        [ ["Usage:", progName, "[OPTION...]", "<cat/pkg[-ver]>"]
        , []
        , ["This utility will scan a Gentoo repository and gather dependency information."]
        , []
        , ["--matching (default when no version is provided)"]
        , ["Looks for dependencies that match the given package atom."]
        , []
        , ["--non-matching (default when version is provided)"]
        , ["Looks for dependency constraints that would reject the provided"]
        , ["package/version. For example:", progName, "dev-haskell/network-3.2 would"]
        , ["match \"<dev-haskell/network-3.2\" as a problematic dependency."]
        ]

    options :: [OptDescr Mode]
    options =
        [ Option ['h'] ["help"] (NoArg HelpMode) "Show this help text"
        , Option ['r'] ["repo"]
            (ReqArg (\r -> NormalMode mempty (pure (Repository r)) mempty)
                "REPOSITORY"
            )
            "Limit to a repository (defaults to \"haskell\")"
        , Option [] ["debug"] (NoArg (NormalMode mempty mempty (Any True)))
            "Display debug information"
        , Option [] ["matching"]
            (NoArg (NormalMode (pure Matching) mempty mempty))
            "Look for matching dependencies"
        , Option [] ["non-matching"]
            (NoArg (NormalMode (pure NonMatching) mempty mempty))
            "Look for non-matching relevant dependencies"
        ]
