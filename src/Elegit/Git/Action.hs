{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}

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
import           Universum

-- | The declaration of all posible actions we can do in the git action.
--
-- This describes the data of the action, and whether it can return any value
-- for further computations.
--
-- We can use records later to better comunicate the purpose of each field by
-- providing a name.
--
-- === How to understand this structure:
--
-- Each case consists of the data and the return type
--
--   * @CurrentBranch (Text -> a)@:
--     The `CurrentBranch` has no data associated with it, because we have only 1 field
--     and the last field indicates the result type
--     In the @Text -> a@ we specify that we provide the value of type `Text` to the
--     next computation. TLDR: If you want to provide value of type @t@ from your command
--     directly - follow the @(t -> a)@ placeholder. If you don't provide any value, you
--     should use the @(() -> a)@ type.
--     In haskell @()@ represents the type `Unit` and has only one value @()@.
--
--   * @UpdateConfig Text Text (() -> a)@:
--     Here you can see that the action `UpdateConfig` has 2 fields of type `Text` and
--     no return value.
--     Note as Haskell is lazy, you can simplify any function of type @() -> a@ to just @a@.
--
-- TODO: Use records
data GitF a
    = CurrentBranch (Text -> a)
    | BranchUpstream Text (Maybe Text -> a)
    | Log LogType Text Text ([Text] -> a)
    | Status StatusType ([Text] -> a)
    | StashList ([Text] -> a)

    | ReportInfo Text a
    | PrintText Text a
    deriving stock (Functor)


-- | Represents types of git status output
--
-- `StatusShort` is the same as "--short" option.
data StatusType
    = StatusShort


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

status :: (MonadFree GitF m) => StatusType -> m [Text]
status sType = liftF $ Status sType id

log :: (MonadFree GitF m) => LogType -> Text -> Text -> m [Text]
log lType lBase lTarget = liftF $ Log lType lBase lTarget id

stashList :: (MonadFree GitF m) => m [Text]
stashList = liftF $ StashList id

currentBranch :: (MonadFree GitF m) => m Text
currentBranch = liftF $ CurrentBranch id

branchUpstream :: (MonadFree GitF m) => Text -> m (Maybe Text)
branchUpstream bName = liftF $ BranchUpstream bName id

-- |
--
-- Maybe we should not report content, but rather format it as info.
reportInfo :: (MonadFree GitF m) => Text -> m ()
reportInfo content = liftF $ ReportInfo content ()

print :: (MonadFree GitF m) => Text -> m ()
print content = liftF $ PrintText content ()


-- Derived actions


freshestDefaultBranch :: (MonadFree GitF m) => m Text
freshestDefaultBranch = do
    -- TODO: Port elegant git logic
    return "main"
