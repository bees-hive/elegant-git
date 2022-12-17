module Elegit.Cli.Action.ShowWork where

import           Control.Monad.Free.Class
import qualified Data.Text                as T
import qualified Elegit.Git.Action        as GA
import           Fmt
import           Universum

-- | Exectuion description of the AquireRepository action
cmd :: (MonadFree GA.GitF m) => m ()
cmd = do
    currentBranch <- GA.currentBranch
    mCurrentUpstream <- GA.branchUpstream currentBranch
    branchWithLatestChanges <- GA.freshestDefaultBranch
    logs <- GA.log GA.LogOneLine branchWithLatestChanges currentBranch
    changes <- GA.status GA.StatusShort
    stashes <- GA.stashList

    GA.reportInfo ">>> Branch refs:"
    GA.reportInfo (fmt "local: "+|currentBranch|+"")
    case mCurrentUpstream of
      Just currentUpstream -> GA.reportInfo (fmt "remote: "+|currentUpstream|+"")
      Nothing -> pass

    GA.reportInfo ""

    unless (null logs) $ do
        GA.reportInfo (fmt ">>> New commits (comparing to "+|branchWithLatestChanges|+" branch):")
        GA.print $ T.intercalate "\n" logs
        GA.reportInfo ""

    unless (null changes) $ do
        GA.reportInfo "Uncommitted modifications:"
        GA.print $ T.intercalate "\n" changes
        GA.reportInfo ""

    unless (null stashes) $ do
        GA.reportInfo ">>> Available stashes:"
        GA.print $ T.intercalate "\n" stashes
