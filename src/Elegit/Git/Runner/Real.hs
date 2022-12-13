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
    (eCode, outputBS, errBS) <- readProcess $ proc "git" (toString <$> words cmd)
    case eCode of
      ExitFailure _ -> throwString $ decodeUtf8 errBS
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
    GA.DefaultUsername next ->
        return $ next "My Name"
    GA.DefaultEmail next ->
        return $ next "example@gmail.com"
    GA.DefaultEditor next ->
        return $ next "nvim"

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

    GA.Prompt name defM next -> do
        let prompt = case defM of
                        Just def -> name <> " {" <> def <> "}"
                        Nothing  -> name
        putText (prompt <> ": ")
        input <- getLine
        let value = if null input then fromMaybe "" defM else input
        return $ next value
    GA.UpdateConfig name value next -> do
        putTextLn $ "git config " <> name <> " " <> value
        return next
    GA.CloneRepository repo next -> do
        putTextLn $ "git clone " <> repo
        return next
