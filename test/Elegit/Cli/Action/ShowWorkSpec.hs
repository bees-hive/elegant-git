module Elegit.Cli.Action.ShowWorkSpec where

import qualified Elegit.Cli.Action.ShowWork  as ShowWork
import           Elegit.Git.Runner.Simulated
import           Test.Hspec
import           Universum

spec :: Spec
spec = do
  describe "cmd" $ do
    it "executes expected list of commands" $ do
      collectImpureCommands ShowWork.cmd `shouldBe`
        [ ReportInfo ">>> Branch refs:"
        , ReportInfo "local: current"
        , ReportInfo "remote: origin"
        , ReportInfo ""
        , ReportInfo ">>> New commits (comparing to origin/main branch):"
        , PrintText "commit 1"
        , ReportInfo ""
        , ReportInfo "Uncommitted modifications:"
        , PrintText "M t.txt"
        , ReportInfo ""
        , ReportInfo ">>> Available stashes:"
        , PrintText "stash@{0}: WIP on current: fc84d95"
        ]
