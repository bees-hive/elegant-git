module Elegit.Git.Runner.Real where

import           Control.Exception.Safe    (throwString)
import           Control.Monad.Free.Church
import           Control.Monad.HT
import qualified Elegit.Git.Action         as GA
import           Elegit.Git.Exec           (GitCommand (..), MonadGitExec (execGit, gLine, pText, pTextLn))
import           Fmt
import           Universum                 as U


-- | Execute the action in the real world.
executeGit :: (MonadCatch m, MonadGitExec m) => GA.FreeGit () -> m ()
executeGit = foldF executeGitF


-- | Interpreter for the real world
--
-- Currently just prints the resulting commands without any execution.
--
executeGitF :: (MonadCatch m, MonadGitExec m) => GA.GitF a -> m a
executeGitF arg = case arg of
  GA.CurrentBranch gc next -> do
    mCurrentBranch <- execGit (GCCB gc)
    case mCurrentBranch of
      Nothing -> throwString "No branch found. Seems like this repository was not initialized fully."
      Just currentBranch ->
        return $ next currentBranch

  GA.BranchUpstream gc next -> do
    mUpstreamBranch <- execGit (GCBU gc)
    return $ next mUpstreamBranch

  GA.Log gc next -> do
    logs <- lines . fromMaybe "" <$> execGit (GCL gc)
    return $ next logs
  GA.Status gc next -> do
    changes <- lines . fromMaybe "" <$> execGit (GCS gc)
    return $ next changes
  GA.StashList gc next -> do
    stashes <- lines . fromMaybe "" <$> execGit (GCSL gc)
    return $ next stashes

  GA.GPGListKeys gc next -> do
    mGpgKeys <- execGit (GCGKL gc)
    return $ next (mGpgKeys >>= nonEmpty . lines)
  GA.AliasesToRemove gc next -> do
    oldAliasesM <- execGit (GCATR gc)
    return $ next (oldAliasesM >>= nonEmpty . lines)
  GA.ReadConfig gc next -> do
    next <$> execGit (GCRC gc)
  GA.SetConfig gc next -> do
    U.void $ execGit (GCSC gc)
    return next
  GA.UnsetConfig gc next -> do
    U.void $ execGit (GCUC gc)
    return next
  GA.Prompt prompt pDefaultM next -> do
    let
      askPrompt = do
        pText (colored Purple Normal message)
        gLine

      message :: Text
      message =
        case pDefaultM of
          Just pDefault -> fmt ""+|prompt|+" {"+|pDefault|+"}: "
          Nothing       -> fmt ""+|prompt|+": "

    answer <-
      case pDefaultM of
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
    pTextLn content
    return next

colored :: Color -> FontStyle -> Text -> Text
colored color style content =
  fmt "\x1b["+||fontStyleCode style||+";"+||colorCode color||+"m"+|content|+"\x1b[0m"


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
