{-# LANGUAGE QuasiQuotes #-}
module Elegit.Cli.Action.ShowWork
    ( cli
    , cmd
    ) where

import           Control.Monad.Free.Class
import           Data.String.QQ
import qualified Data.Text                       as T
import           Elegit.Cli.Command
import qualified Elegit.Git.Action               as GA
import           Fmt
import           Options.Applicative
import qualified Options.Applicative.Help.Pretty as OA
import           Universum


purpose :: OA.Doc
purpose = OA.text "Prints HEAD state."

description :: OA.Doc
description = OA.string [s|
Prints HEAD state by displaying local and remote-tracking (if available) refs,
commits that aren't in the default development branch, uncommitted
modifications, and available stashes.

Approximate commands flow is
```bash
==>> git elegant show-work
git log --oneline master..@
git status --short
git stash list
```|]


cli :: Mod CommandFields ElegitCommand
cli = command "show-work" $ info (pure ShowWorkCommand) $
    mconcat [ progDescDoc (Just purpose )
            , footerDoc (Just description )
            ]


-- | Execution description of the ShowWork action
cmd :: (MonadFree GA.GitF m) => m ()
cmd = do
    whenJustM GA.currentBranch $ \currentBranch -> do
        mCurrentUpstream <- GA.branchUpstream currentBranch
        branchWithLatestChanges <- GA.freshestDefaultBranch
        logs <- GA.log GA.LogOneLine branchWithLatestChanges currentBranch
        changes <- GA.status GA.StatusShort
        stashes <- GA.stashList

        GA.print =<< GA.formatInfo ">>> Branch refs:"
        GA.print =<< GA.formatInfo (fmt "local: "+|currentBranch|+"")
        case mCurrentUpstream of
          Nothing -> pass
          Just currentUpstream ->
              GA.print =<< GA.formatInfo (fmt "remote: "+|currentUpstream|+"")

        GA.print ""

        unless (null logs) $ do
            GA.print =<< GA.formatInfo (fmt ">>> New commits (comparing to "+|branchWithLatestChanges|+" branch):")
            GA.print $ T.intercalate "\n" logs
            GA.print ""

        unless (null changes) $ do
            GA.print =<< GA.formatInfo ">>> Uncommitted modifications:"
            GA.print $ T.intercalate "\n" changes
            GA.print ""

        unless (null stashes) $ do
            GA.print =<< GA.formatInfo ">>> Available stashes:"
            GA.print $ T.intercalate "\n" stashes
