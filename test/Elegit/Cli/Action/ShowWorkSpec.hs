module Elegit.Cli.Action.ShowWorkSpec where

import qualified Data.List.NonEmpty          as NE
import qualified Elegit.Cli.Action.ShowWork  as ShowWork
import           Elegit.Git.Runner.Simulated
import           Lens.Micro
import           Lens.Micro.Mtl
import           Test.Hspec
import           Universum                   hiding (view, (%~), (.~), (^.))


defaultRepository :: GRepository
defaultRepository =
  let
    commit = GCommit
      { _gcName = "Init commit"
      }
    mainBranch = GBranch
      { _gbName = "main"
      , _gbUpstream = Nothing
      , _gbCommit = pure commit
      }
    currentBranch = GBranch
      { _gbName = "haskell"
      , _gbUpstream = Nothing
      , _gbCommit = pure commit
      }
  in GRepository
     { _grRemotes = []
     , _grBranches = [mainBranch, currentBranch]
     , _grCurrentBranch = currentBranch^.gbName
     , _grModifiedFiles = []
     , _grUnstagedFiles = []
     , _grStashes = [ ]
     }

spec :: Spec
spec = do
  describe "cmd" $ do
    it "prints state of HEAD branch" $ do
      runGitActionPure defaultRepository ShowWork.cmd `shouldBe`
        ( defaultRepository
        , [ ReportInfo ">>> Branch refs:"
          , ReportInfo "local: haskell"
          , ReportInfo ""
          ]
        )
    it "print status when files changed" $ do
      let
        repo = defaultRepository & grModifiedFiles .~ ["app/Main.hs"]
                                 & grUnstagedFiles .~ ["tmp.txt"]
      runGitActionPure repo ShowWork.cmd `shouldBe`
        ( repo
        , [ ReportInfo ">>> Branch refs:"
          , ReportInfo "local: haskell"
          , ReportInfo ""
          , ReportInfo ">>> Uncommitted modifications:"
          , PrintText "M app/Main.hs"
          , PrintText "?? tmp.txt"
          , ReportInfo ""
          ]
        )
    it "prints remote" $ do
      let
        repo = defaultRepository & grBranches.mapped.gbUpstream ?~ "origin/haskell"
      runGitActionPure repo ShowWork.cmd `shouldBe`
        ( repo
        , [ ReportInfo ">>> Branch refs:"
          , ReportInfo "local: haskell"
          , ReportInfo "remote: origin/haskell"
          , ReportInfo ""
          ]
        )
    it "prints stash" $ do
      let
        stash = GStash
          { _gsName = "WIP"
          , _gsBranchName = "haskell"
          }
        repo = defaultRepository & grStashes .~ [stash]

      runGitActionPure repo ShowWork.cmd `shouldBe`
        ( repo
        , [ ReportInfo ">>> Branch refs:"
          , ReportInfo "local: haskell"
          , ReportInfo ""
          , ReportInfo ">>> Available stashes:"
          , PrintText "stash@{0}: WIP on haskell"
          ]
        )
    it "prints log" $ do
      let
        newCommit = GCommit {_gcName = "Updates"}

        repo = defaultRepository & grBranches . each . filtered ((== "haskell") . view gbName) . gbCommit %~ NE.cons newCommit

      runGitActionPure repo ShowWork.cmd `shouldBe`
        ( repo
        , [ ReportInfo ">>> Branch refs:"
          , ReportInfo "local: haskell"
          , ReportInfo ""
          , ReportInfo ">>> New commits (comparing to main branch):"
          , PrintText "Updates"
          , ReportInfo ""
          ]
        )
