module Elegit.Git.Runner.Real where

import           Control.Exception.Safe    (throwString)
import           Control.Monad.Free.Church
import           Control.Monad.HT
import           Data.Text                 (stripEnd)
import qualified Elegit.Git.Action         as GA
import           Fmt
import           System.IO                 (hFlush)
import           System.Process.Typed      (ExitCode (ExitFailure, ExitSuccess), proc, readProcess)
import           Universum                 as U


data Color
  = Green
  | Blue
  | Purple

colorCode :: Color -> Int
colorCode Green  = 32
colorCode Blue   = 34
colorCode Purple = 35


data FontStyle
  = Normal
  | Bold

fontStyleCode :: FontStyle -> Int
fontStyleCode Normal = 0
fontStyleCode Bold   = 1


runGit :: (MonadIO m) => Text -> m (Maybe Text)
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


colored :: Color -> FontStyle -> Text -> Text
colored color style content =
    fmt "\x1b["+||fontStyleCode style||+";"+||colorCode color||+"m"+|content|+"\x1b[0m"

-- | Interpreter for the real world
--
-- Currently just prints the resulting commands without any execution.
--
executeGitF :: (MonadCatch m, MonadIO m) => GA.GitF a -> m a
executeGitF arg = case arg of
    GA.CurrentBranch next -> do
        mCurrentBranch <- runGit "rev-parse --abbrev-ref @"
        case mCurrentBranch of
          Nothing -> throwString "No branch found. Seems like this repository was not initialized fully."
          Just currentBranch ->
            return $ next currentBranch

    GA.BranchUpstream branch next -> do
        mUpstreamBranch <- runGit (fmt "rev-parse --abbrev-ref "+|branch|+"@{upstream}")
        return $ next mUpstreamBranch

    GA.Log lType base target next -> do
        let
            logArg :: Text
            logArg = case lType of
                       GA.LogOneLine -> "--oneline"

        logs <- lines . fromMaybe "" <$> runGit (fmt "-c color.ui=always log "+|logArg|+" "+|base|+".."+|target|+"")
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

    GA.AliasesToRemove cScope next -> do
        let
            scope :: Text
            scope = case cScope of
                      GA.LocalConfig  -> "--local"
                      GA.GlobalConfig -> "--global"
                      GA.AutoConfig   -> ""

        oldAliasesM <- runGit (fmt "config "+|scope|+" --name-only --get-regexp \"^alias.\" \"^elegant ([-a-z]+)$\"")
        return $ next (oldAliasesM >>= nonEmpty . lines)
    GA.ReadConfig cScope cName next -> do
        let
            scope :: Text
            scope = case cScope of
                      GA.LocalConfig  -> "--local"
                      GA.GlobalConfig -> "--global"
                      GA.AutoConfig   -> ""

        next <$> runGit (fmt "config "+|scope|+" --get "+|cName|+"")
    GA.SetConfig cScope cName cValue next -> do
        let
            scope :: Text
            scope = case cScope of
                      GA.LocalConfig  -> "--local"
                      GA.AutoConfig   -> "--local"
                      GA.GlobalConfig -> "--global"
        U.void $ runGit (fmt "config "+|scope|+" "+|cName|+" "+|cValue|+"")
        return next
    GA.UnsetConfig cScope cName next -> do
        let
            scope :: Text
            scope = case cScope of
                      GA.LocalConfig  -> "--local"
                      GA.AutoConfig   -> "--local"
                      GA.GlobalConfig -> "--global"
        U.void $ runGit (fmt "config "+|scope|+" --unset "+|cName|+"")
        return next
    GA.Prompt prompt pDefaultM next -> do
        let
            askPrompt = do
                putText (colored Purple Normal message)
                liftIO $ hFlush stdout
                getLine

            message :: Text
            message =
                case pDefaultM of
                  Just pDefault -> fmt ""+|prompt|+" {"+|pDefault|+"}: "
                  Nothing       -> fmt ""+|prompt|+": "

        answer <- case pDefaultM of
              Nothing -> until (not . null) askPrompt
              Just pDefault -> do
                  answer <- askPrompt
                  if null answer
                     then return pDefault
                     else return answer
        return $ next answer

    GA.FormatInfo content next -> do
        return $ next $ colored Green Normal content
    GA.FormatCommand content next -> do
        return $ next $ colored Green Normal "==>>" <> " " <> colored Blue Bold content

    GA.PrintText content next -> do
        putTextLn content
        return next
