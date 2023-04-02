{-# LANGUAGE QuasiQuotes #-}
module Elegit.Cli.Action.InitRepository
    ( cli
    , cmd
    ) where

import           Control.Monad.Free.Class
import           Data.String.QQ
import qualified Elegit.Cli.Action.AcquireRepository as AcquireRepository
import           Elegit.Cli.Command
import qualified Elegit.Git.Action                   as GA
import           Options.Applicative
import qualified Options.Applicative.Help.Pretty     as OA
import           Universum


purpose :: OA.Doc
purpose = OA.text "Initializes a new repository and configures it."

description :: OA.Doc
description = OA.string [s|
Creates an empty Git repository (or reinitialize an existing one), runs its
configuration, and creates an initial empty commit.

Approximate commands flow is
```bash
==>> git elegant init-repository
git init
git elegant acquire-repository
git commit --allow-empty --file a-message-of-initial-commit
git show
```|]


initialCommitMessage :: Text
initialCommitMessage = [s|
Add initial empty commit

This commit is the first commit in this working tree. It does not have
any changes. However, it simplifies further work at least in the
following cases:
- it's possible to create a branch now
- it's possible to manage the second commit if it requires some
polishing after creation

This commit is created automatically by Elegant Git after the
initialization of a new repository.|]


cli :: Mod CommandFields ElegitCommand
cli = command "init-repository" $ info (pure InitRepositoryCommand) $
    mconcat [ progDescDoc (Just purpose )
            , footerDoc (Just description )
            ]


cmd :: (MonadFree GA.GitF m) => m ()
cmd = do
    GA.initRepositoryVerbose
    AcquireRepository.cmd
    GA.addInitialCommitVerbose initialCommitMessage
    GA.print . unlines =<< GA.showVerbose GA.ShowHead
