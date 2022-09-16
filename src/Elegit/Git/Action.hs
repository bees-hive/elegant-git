{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Elegit.Git.Action
--
-- This module provides `the` way to build the exectuion tree for any action
-- we can think off.
-- To make the life easier for us, we can leverage the functional concept
-- `Free Monad`. `Free` actually just gives a way for us to lift any `Functor f`
-- to use it in the monad context. Also the cool thing, is that it's represented
-- as tree of arbitrarely nested computations.
--
-- This feature of `Free` gives us an easy way to describe the potential execution
-- and build any number of interpreters we want. This way we can mock any command
-- for testing purposes or even create the summary of the commands we will execute.
--
-- Some notes:
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
-- How to understand this structure:
--   Each case consists of the data and the return type
--
--   For example:
--   * `CurrentBranch (Text -> a)`
--     The CurrentBranch has no data associated with it, because we have only 1 field
--     and the last field indicates the result type
--     In the `(Text -> a)` we specify that we provide the value of type `Text` to the 
--     next computation. TLDR: If you want to provide value of type `t` from your command
--     directly - follow the `(t -> a)` placeholder. If you don't provide any value, you
--     should use the `(() -> a)` type.
--     In haskell `()` represents the type `Unit` and has only one value `()`.
--  
--   * `UpdateConfig Text Text (() -> a)`
--     Here you can see that the action `UpdateConfig` has 2 fields of type `Text` and
--     no return value. 
--     Note as Haskell is lazy, you can simplify any function of type `() -> a` to just `a`.
--
data GitF a
    = UpdateConfig Text Text a
    | CurrentBranch (Text -> a)
    | Prompt Text (Text -> a)
    | CloneRepository Text a
    deriving stock (Functor)

-- | Type alias to the Free GitF monad.
type FreeGit t = F GitF t

-----------------------------------------------------------------------------
-- | You should consider following code as a boilerplate
--
-- Each command should have the associated function to simplify the usage of this API.
-- This can be generated with haskell `TemplateHaskell` feature, but it's better to have
-- it here to understand what we do here
--
-- Every function just tells us that we can use them in any `Monad m` which has a
-- `MonadFree` implementation. This is just a fancy way to make the code more generic in
-- terms of execution context.
--
-- Some notes:
-- * When our function has a continuation of type `(() -> a)`, you simply pass () as the
--   value.
-- * Otherwise just use `id` function.
-----------------------------------------------------------------------------

updateConfig :: (MonadFree GitF m) => Text -> Text -> m ()
updateConfig name value = liftF $ UpdateConfig name value ()

cloneRepository :: (MonadFree GitF m) => Text -> m ()
cloneRepository repo = liftF $ CloneRepository repo ()

prompt :: (MonadFree GitF m) => Text -> m Text
prompt name = liftF $ Prompt name id

currentBranch :: (MonadFree GitF m) => m Text
currentBranch = liftF $ CurrentBranch id
