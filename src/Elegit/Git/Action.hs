{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Elegit.Git.Action
--
-- This module provides the way to build the exectuion tree for any action
-- we can think of.
-- To make the life easier for us, we can leverage the functional concept
-- "Free Monad". `Free` gives us a way to lift any `Functor f` to use it in the
-- monadic context.
--
-- The cool thing about `Free`, is that it's represented as tree of arbitrarely
-- nested computations.
--
-- This feature of `Free` gives us an easy way to describe the potential execution
-- and build any number of interpreters we want. This way we can mock any command
-- for testing purposes or create the summary of the commands we will execute.
--
-- Some notes:
--
-- * "Church" is the implementation of the `Free` which has a better performance
--   in the process of building the tree. Basically we use fancy way to build
--   our tree only once.
--
-----------------------------------------------------------------------------
module Elegit.Git.Action where


import           Control.Monad.Free
import           Control.Monad.Free.Church
import qualified Data.Text                 as T
import           Fmt
import           Universum                 hiding (print)

-- TODO: maybe, cover with tests
class RenderGitCommand c where
  renderGC :: c -> Text

data GCurrentBranchData
  = GCurrentBranchData

instance RenderGitCommand GCurrentBranchData where
  renderGC _ = "rev-parse --abbrev-ref @"

newtype GBranchUpstreamData
  = GBranchUpstreamData { branch :: Text }

instance RenderGitCommand GBranchUpstreamData where
  renderGC (GBranchUpstreamData branchName) = "rev-parse --abbrev-ref "+|branchName|+"@{upstream}"

data GLogData
  = GLogData
      { logType :: LogType
      , base    :: Text
      , target  :: Text
      }
instance RenderGitCommand GLogData where
  renderGC (GLogData lType baseName targetName) = "log "+|logArg|+" "+|baseName|+".."+|targetName|+""
    where
      logArg :: Text
      logArg = case lType of
                 LogOneLine -> "--oneline"

newtype GStatusData
  = GStatusData { statusType :: StatusType }

instance RenderGitCommand GStatusData where
  renderGC (GStatusData sType) = "status "+|statusFormat|+""
    where
      statusFormat :: Text
      statusFormat = case sType of
                       StatusShort -> "--short"

data GStashListData
  = GStashListData
instance RenderGitCommand GStashListData where
  renderGC _ = "stash list"

data GReadConfigData
  = GReadConfigData
      { scope :: ConfigScope
      , key   :: Text
      }

instance RenderGitCommand GReadConfigData where
  renderGC (GReadConfigData cScope cName) = "config "+|scopeText|+" --get "+|cName|+""
    where
      scopeText :: Text
      scopeText = case cScope of
                LocalConfig  -> "--local"
                GlobalConfig -> "--global"
                AutoConfig   -> ""

data GSetConfigData
  = GSetConfigData
      { scope :: ConfigScope
      , key   :: Text
      , value :: Text
      }

instance RenderGitCommand GSetConfigData where
  renderGC (GSetConfigData cScope cName cValue) = "config "+|scopeText|+" "+|cName|+" "+|cValue|+""
    where
      scopeText :: Text
      scopeText = case cScope of
                GlobalConfig -> "--global"
                LocalConfig  -> "--local"
                AutoConfig   -> "--local"

data GUnsetConfigData
  = GUnsetConfigData
      { scope :: ConfigScope
      , key   :: Text
      }

instance RenderGitCommand GUnsetConfigData where
  renderGC (GUnsetConfigData cScope cName) = "config "+|scopeText|+" --unset "+|cName|+""
    where
      scopeText :: Text
      scopeText = case cScope of
                GlobalConfig -> "--global"
                LocalConfig  -> "--local"
                AutoConfig   -> "--local"

newtype GAliasesToRemoveData
  = GAliasesToRemoveData { scope :: ConfigScope }

instance RenderGitCommand GAliasesToRemoveData where
  renderGC (GAliasesToRemoveData cScope) = "config "+|scopeText|+" --name-only --get-regexp \"^alias.\" \"^elegant ([-a-z]+)$\""
    where
      scopeText :: Text
      scopeText = case cScope of
                GlobalConfig -> "--global"
                LocalConfig  -> "--local"
                AutoConfig   -> ""

-- | The declaration of all posible actions we can do in the git action.
--
-- This describes the data of the action, and whether it can return any value
-- for further computations.
--
-- We can use records later to better comunicate the purpose of each field by
-- providing a name.
data GitF a
  = CurrentBranch GCurrentBranchData (Text -> a)
  | BranchUpstream GBranchUpstreamData (Maybe Text -> a)
  | Log GLogData ([Text] -> a)
  | Status GStatusData ([Text] -> a)
  | StashList GStashListData ([Text] -> a)
  | ReadConfig GReadConfigData (Maybe Text -> a)
  | AliasesToRemove GAliasesToRemoveData (Maybe (NonEmpty Text) -> a)
  | SetConfig GSetConfigData a
  | UnsetConfig GUnsetConfigData a
  | Prompt Text (Maybe Text) (Text -> a)
  | FormatInfo Text (Text -> a)
  | FormatCommand Text (Text -> a)
  | PrintText Text a
  deriving stock (Functor)


-- | Represents types of git status output
--
-- `StatusShort` is the same as "--short" option.
data StatusType
  = StatusShort


data ConfigScope
  = LocalConfig
  | GlobalConfig
  | AutoConfig


-- | Represents types of git log output
--
-- `LogOneLine` is the same as "--oneline" option.
data LogType
  = LogOneLine


-- | Type alias to the `Free` `GitF` monad.
type FreeGit t = F GitF t

-- | You should consider following code as a boilerplate
--
-- Each command should have the associated function to simplify the usage of this API.
-- This can be generated with haskell `TemplateHaskell` feature, but it's better to have
-- it here to understand what we do here
--
-- Every function just tells us that we can use them in any @Monad m@ which has a
-- `MonadFree` implementation. This is just a fancy way to make the code more generic in
-- terms of execution context.
--
-- Some notes:
--
-- * When our function has a continuation of type @(() -> a)@, you simply pass @()@ as the
--   value.
-- * Otherwise just use `id` function.

status :: MonadFree GitF m => StatusType -> m [Text]
status sType = liftF $ Status (GStatusData sType) id

log :: MonadFree GitF m => LogType -> Text -> Text -> m [Text]
log lType lBase lTarget = liftF $ Log (GLogData lType lBase lTarget) id

stashList :: MonadFree GitF m => m [Text]
stashList = liftF $ StashList GStashListData id

currentBranch :: MonadFree GitF m => m Text
currentBranch = liftF $ CurrentBranch GCurrentBranchData id

branchUpstream :: MonadFree GitF m => Text -> m (Maybe Text)
branchUpstream bName = liftF $ BranchUpstream (GBranchUpstreamData bName) id

readConfig :: MonadFree GitF m => ConfigScope -> Text -> m (Maybe Text)
readConfig cScope cName = liftF $ ReadConfig (GReadConfigData cScope cName) id

-- TODO: Check if it's better or even possible to get all configurations and filter them ourself.
-- This would improve testability of this, as now we rely on the fact that we make a correct cli call
-- to find config with regex in the `Real.hs`.
aliasesToRemove :: MonadFree GitF m => ConfigScope -> m (Maybe (NonEmpty Text))
aliasesToRemove cScope = liftF $ AliasesToRemove (GAliasesToRemoveData cScope) id

setConfig :: MonadFree GitF m => ConfigScope -> Text -> Text -> m ()
setConfig cScope cName cValue = liftF $ SetConfig (GSetConfigData cScope cName cValue) ()

unsetConfig :: MonadFree GitF m => ConfigScope -> Text -> m ()
unsetConfig cScope cName = liftF $ UnsetConfig (GUnsetConfigData cScope cName) ()

promptDefault :: MonadFree GitF m => Text -> Maybe Text -> m Text
promptDefault pText pDefault = liftF $ Prompt pText pDefault id

formatInfo :: MonadFree GitF m => Text -> m Text
formatInfo content = liftF $ FormatInfo content id

formatCommand :: MonadFree GitF m => Text -> m Text
formatCommand cmd = liftF $ FormatCommand cmd id

print :: MonadFree GitF m => Text -> m ()
print content = liftF $ PrintText content ()

-- Derived actions

formatGitCommand :: (RenderGitCommand gc, MonadFree GitF m) => gc -> m Text
formatGitCommand gc = formatCommand ("git "+|renderGC gc|+"")

setConfigVerbose :: MonadFree GitF m => ConfigScope -> Text -> Text -> m ()
setConfigVerbose cScope cName cValue = do
  setConfig cScope cName cValue
  print =<< formatGitCommand (GSetConfigData cScope cName cValue)

unsetConfigVerbose :: MonadFree GitF m => ConfigScope -> Text -> m ()
unsetConfigVerbose cScope cName = do
  unsetConfig cScope cName
  print =<< formatGitCommand (GUnsetConfigData cScope cName)

freshestDefaultBranch :: MonadFree GitF m => m Text
freshestDefaultBranch = do
    -- TODO: Port bash logic
    return "main"

isGitAcquired :: MonadFree GitF m => m Bool
isGitAcquired = do
    isJust <$> readConfig LocalConfig "elegant.acquired"

formatInfoBox :: (MonadFree GitF m) => Text -> m Text
formatInfoBox content =
  formatInfo
    (""+|box|+"\n== "+|content|+" ==\n"+|box|+"")
    where
      contentLength :: Int
      contentLength = length content
      box = T.replicate (3 + contentLength + 3) "="


removeAliases :: MonadFree GitF m => ConfigScope -> m ()
removeAliases cScope = do
    whenJustM (aliasesToRemove cScope) $ \aliases -> do
      print =<< formatInfo "Removing old Elegant Git aliases..."
      mapM_ (unsetConfigVerbose cScope) aliases


removeObsoleteConfiguration :: MonadFree GitF m => ConfigScope -> m ()
removeObsoleteConfiguration cScope = do
  print =<< formatInfoBox "Removing obsolete configurations..."
  whenM isGitAcquired $ do
    print =<< formatInfo "Removing old Elegnat Git configuration keys..."
    unsetConfigVerbose cScope "elegant.acquired"

  removeAliases cScope
