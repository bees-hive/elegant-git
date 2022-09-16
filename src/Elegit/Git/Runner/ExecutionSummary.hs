{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
module Elegit.Git.Runner.ExecutionSummary
    ( collectImpureCommands
    , GitCommand(..)
    ) where

import           Control.Monad.Free.Church
import           Control.Monad.Writer.Strict
import           Data.DList                  as DList
import qualified Elegit.Git.Action           as GA
import           Universum

-- | Describes all the metrics we collect from the git action execution
data GitCommand
    = UpdateConfigCommand Text Text
    | CloneRepositoryCommand Text
    deriving stock (Show, Eq)

-- | Collects a summary of execution as a list of `GitCommand`s.
collectImpureCommands :: GA.FreeGit () -> [GitCommand]
collectImpureCommands action =
    DList.toList $ runIdentity $ execWriterT $ foldF collectImpureCommandsF action

-- | Interpreter of GitF which collects the summary of exection in `Writer` monad.
--
-- Each branch should return the value of type a, which can be obtained by calling
-- the `next` function.
--
-- We use `tell` function to store the command in `DList GitCommand`. `DList` can be treated
-- as just plain list (`[]`) but that has O(1) `append` operation instead of the O(n) of the `[]`.
--
-- Note that we are in the context of the `Writer` monad and we need to wrap the value `a` in 
-- `Writer`. To lift any value into a monad you should use `return`.
collectImpureCommandsF :: GA.GitF a -> Writer (DList GitCommand) a
collectImpureCommandsF cmd = case cmd of
    GA.CurrentBranch next ->
        return $ next "current"
    GA.Prompt _ next -> do
        return $ next "asdasd"
    GA.UpdateConfig name value next -> do
        tell $ singleton $ UpdateConfigCommand name value
        return next
    GA.CloneRepository repo next -> do
        tell $ singleton $ CloneRepositoryCommand repo
        return next
