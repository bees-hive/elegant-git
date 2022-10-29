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
    GA.DefaultUsername next ->
        return $ next "My Name"
    GA.DefaultEmail next ->
        return $ next "example@gmail.com"
    GA.DefaultEditor next ->
        return $ next "vim"
    GA.CurrentBranch next -> do
        return $ next "current"

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
