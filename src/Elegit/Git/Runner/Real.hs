module Elegit.Git.Runner.Real where

import           Control.Monad.Free.Church
import qualified Elegit.Git.Action         as GA
import           Universum

-- | Execute the action in the real world.
executeGit :: GA.FreeGit () -> IO ()
executeGit = foldF executeGitF

-- | Interpreter for the real world
--
-- Currently just prints the resulting commands without any execution.
--
executeGitF :: GA.GitF a -> IO a
executeGitF arg = case arg of
    GA.CurrentBranch next -> do
        return $ next "current"
    GA.Prompt name next -> do
        putText (name <> ": ")
        next <$> getLine
    GA.UpdateConfig name value next -> do
        putTextLn $ "git config " <> name <> " " <> value
        return next
    GA.CloneRepository repo next -> do
        putTextLn $ "git clone " <> repo
        return next
