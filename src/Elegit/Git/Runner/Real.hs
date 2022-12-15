module Elegit.Git.Runner.Real where

import           Control.Exception.Safe    (throwString)
import           Control.Monad.Free.Church
import           Data.Text                 (stripEnd)
import qualified Elegit.Git.Action         as GA
import           Fmt
import           System.Process.Typed      (ExitCode (ExitFailure, ExitSuccess),
                                            proc, readProcess)
import           Universum

runGit :: (MonadCatch m, MonadIO m) => Text -> m (Maybe Text)
runGit cmd = do
    (eCode, outputBS, _errBS) <- readProcess $ proc "git" (toString <$> words cmd)
    case eCode of
      ExitFailure _ -> do
          -- TODO: Know what error is expected and log unexpected ones.
          return Nothing
      ExitSuccess -> do
          let output = stripEnd $ decodeUtf8 outputBS
          return $ if null output then Nothing else Just output

-- | Execute the action in the real world.
executeGit :: (MonadCatch m, MonadIO m) => GA.FreeGit () -> m ()
executeGit = foldF executeGitF

-- | Interpreter for the real world
--
-- Currently just prints the resulting commands without any execution.
--
executeGitF :: (MonadCatch m, MonadIO m) => GA.GitF a -> m a
executeGitF arg = case arg of
    GA.CurrentBranch next -> do
        mCurrentBranch <- runGit "rev-parse --abbrev-ref @"
        case mCurrentBranch of
          Nothing -> throwString "No current branch found"
          Just currentBranch ->
            return $ next currentBranch

    GA.BranchUpstream branch next -> do
        mUpstreamBranch <- runGit (fmt "rev-parse --abbrev-ref "+|branch|+"@{upstream}")
        return $ next mUpstreamBranch

    GA.Log lType target next -> do
        let
            logArg :: Text
            logArg = case lType of
                       GA.LogOneLine -> "--oneline"

        logs <- lines . fromMaybe "" <$> runGit (fmt "-c color.ui=always log "+|logArg|+" "+|target|+"")
        return $ next logs
    GA.Status sType next -> do
        let
            statusFormat :: Text
            statusFormat = case sType of
                             GA.StatusShort -> "--short"

        changes <- lines . fromMaybe "" <$> runGit (fmt "-c color.status=always status "+|statusFormat|+"")
        return $ next changes
    GA.StashList next -> do
        stashes <- lines . fromMaybe "" <$> runGit "stash list"
        return $ next stashes

    GA.ReportInfo content next -> do
        putTextLn (fmt "\x1b[32m"+|content|+"\x1b[0m")
        return next
    GA.PrintText content next -> do
        putTextLn content
        return next
