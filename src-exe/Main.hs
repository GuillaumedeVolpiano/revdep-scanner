{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase  #-}

module Main where

import           Conduit
import           Control.Monad
import           Control.Monad.Trans.Accum  (AccumT, add, evalAccumT, look)
import qualified Data.Functor               (($>))
import           Data.List.NonEmpty         (NonEmpty (..))
import qualified Data.List.NonEmpty         as NE
import           Data.Maybe                 (fromMaybe, isJust, isNothing)
import           Data.Monoid
import           System.Console.GetOpt
import           System.Directory
import           System.Environment
import           System.Exit                (die, exitSuccess)
import           System.IO                  (hPutStrLn, stderr)

import           Data.Parsable              (runParsable)
import           Distribution.Portage.Types
import           Text.Pretty.Simple         (pPrintForceColor)

import           RevdepScanner              (ConstraintMap, Debug,
                                             MatchMode (..),
                                             Mode (HelpMode, NormalMode), args,
                                             lookupResults, parseAll,
                                             prettyMatches, prettyProblems,
                                             runOpaque, runTransparent)

main :: IO ()
main = do
  pquery <- runEnv pqueryPath
  (ps, mode, repoName, Any d) <- checkArgs
  when d $ print $ pquery : args repoName
  let f =
        if d
          then runTransparent
          else runOpaque
  (_, out, _) <- f pquery (args repoName)
  Right (m :: ConstraintMap) <- pure $ parseAll out
  when d $ pPrintForceColor m
  let ls =
        ps >>= \p -> do
          let r = lookupResults mode p m
          case mode of
            Matching    -> prettyMatches p r
            NonMatching -> prettyProblems p r
  putStr $ unlines $ NE.toList ls

-- Environment stuff
type Env = AccumT (First FilePath) IO

runEnv :: Env a -> IO a
runEnv = flip evalAccumT mempty

-- | Find the path to the @pquery@ executable or throw an error. Caches the
--   result in the case of a success.
pqueryPath :: Env FilePath
pqueryPath =
  look >>= \case
    First (Just p) -> pure p
    First Nothing ->
      liftIO (findExecutable "pquery") >>= \case
        Just p -> (add (pure p) Data.Functor.$> p)
        Nothing ->
          liftIO
            $ die
                "Could not find pquery executable. Install sys-apps/pkgcore first."

checkArgs :: IO (NonEmpty Package, MatchMode, Repository, Debug)
checkArgs = do
  progName <- getProgName
  argv <- getArgs
  let err str = showHelp progName *> die ("error: " ++ str)
  case getOpt Permute options argv of
    (_, _, es@(_:_)) -> err (unwords es)
    (ms, as, _) ->
      case (mconcat ms, NE.nonEmpty as) of
        (HelpMode, _) -> showHelp progName *> exitSuccess
        (_, Nothing) ->
          err
            "At least one full package name (and optional \
           \version) required"
        (NormalMode (Last mm) (Last mr) d, Just pStrs) ->
          case traverse (runParsable "command line argument") pStrs of
            Left e -> err $ "Invalid package: " ++ show e
            Right ps -> do
              m <-
                case mm of
                  Just mode -> pure mode
                  Nothing   -> detectMode ps
              pure (ps, m, fromMaybe (Repository "haskell") mr, d)
  where
    showHelp progName = putStrLn (usageInfo (header progName) options)
    header progName =
      unlines
        $ unwords
            <$> [ ["Usage:", progName, "[OPTION...]", "<cat/pkg[-ver]... >"]
                , []
                , [ "This utility will scan a Gentoo repository and gather dependency information."
                  ]
                , []
                , ["--matching (default when no version is provided)"]
                , ["Looks for dependencies that match the given package atom."]
                , []
                , ["--non-matching (default when version is provided)"]
                , [ "Looks for dependency constraints that would reject the provided"
                  ]
                , [ "package/version. For example:"
                  , "`" ++ progName
                  , "dev-haskell/network-3.2` would"
                  ]
                , [ "match \"<dev-haskell/network-3.2\" as a problematic dependency."
                  ]
                ]
    options :: [OptDescr Mode]
    options =
      [ Option ['h'] ["help"] (NoArg HelpMode) "Show this help text"
      , Option
          ['r']
          ["repo"]
          (ReqArg
             (\r -> NormalMode mempty (pure (Repository r)) mempty)
             "REPOSITORY")
          "Limit to a repository (defaults to \"haskell\")"
      , Option
          []
          ["debug"]
          (NoArg (NormalMode mempty mempty (Any True)))
          "Display debug information"
      , Option
          []
          ["matching"]
          (NoArg (NormalMode (pure Matching) mempty mempty))
          "Look for matching dependencies"
      , Option
          []
          ["non-matching"]
          (NoArg (NormalMode (pure NonMatching) mempty mempty))
          "Look for non-matching relevant dependencies"
      ]
    detectMode :: Foldable f => f Package -> IO MatchMode
    detectMode ps
      | all (isJust . getVersion) ps = pure NonMatching
      | all (isNothing . getVersion) ps = pure Matching
      | otherwise = do
        hPutStrLn
          stderr
          "Warning: Mix of versioned and non-versioned \
         \packages were given on the command\n\
         \line. Defaulting to \"non-matching mode\"."
        pure NonMatching
