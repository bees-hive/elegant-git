module Elegit.Git.Runner.Real where

import           Control.Exception.Safe    (throwString)
import           Control.Monad.Free.Church
import           Control.Monad.HT
import qualified Elegit.Git.Action         as GA
import           Elegit.Git.Exec           (MonadGitExec (execGit, gLine, pText, pTextLn))
import           Fmt
import           Universum                 as U


-- | Execute the action in the real world.
executeGit :: (MonadCatch m, MonadGitExec m) => GA.FreeGit () -> m ()
executeGit = foldF executeGitF


-- | Interpreter for the real world
executeGitF :: (MonadCatch m, MonadGitExec m) => GA.GitF a -> m a
executeGitF arg = case arg of
  GA.CurrentBranch gc next -> do
    mCurrentBranch <- execGit gc
    case mCurrentBranch of
      Nothing -> throwString "No branch found. Seems like this repository was not initialized fully."
      Just currentBranch ->
        return $ next currentBranch

  GA.BranchUpstream gc next -> do
    mUpstreamBranch <- execGit gc
    return $ next mUpstreamBranch

  GA.Log gc next -> do
    logs <- fromMaybe [] <$> execGit gc
    return $ next logs
  GA.Status gc next -> do
    changes <- fromMaybe [] <$> execGit gc
    return $ next changes
  GA.StashList gc next -> do
    stashes <- fromMaybe [] <$> execGit gc
    return $ next stashes

  GA.PathToTool gc next -> do
    path <- execGit gc
    return $ next path

  GA.GPGListKeys gc next -> do
    next <$> execGit gc
  GA.AliasesToRemove gc next -> do
    next <$> execGit gc
  GA.ReadConfig gc next -> do
    next <$> execGit gc
  GA.SetConfig gc next -> do
    U.void $ execGit gc
    return next
  GA.UnsetConfig gc next -> do
    U.void $ execGit gc
    return next
  GA.Prompt (GA.PromptConfig prompt pType) next -> do
    let
      askPrompt = do
        pText (colored Purple Normal message)
        gLine

      message :: Text
      message =
        case pType of
          GA.PromptOneTime                 -> fmt ""+|prompt|+": "
          GA.PromptDefault (Just pDefault) -> fmt ""+|prompt|+" {"+|pDefault|+"}: "
          GA.PromptDefault Nothing         -> fmt ""+|prompt|+": "

    answer <-
      case pType of
        GA.PromptOneTime                 -> askPrompt
        GA.PromptDefault Nothing         -> until (not . null) askPrompt
        GA.PromptDefault (Just pDefault) -> do
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
