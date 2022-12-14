module Elegit.Cli.Action.ShowWorkSpec where

import qualified Elegit.Cli.Action.ShowWork  as ShowWork
import           Elegit.Git.Runner.Simulated
import           Lens.Micro
import           Test.Hspec
import           Universum


defaultRepository =
  let branch = GBranch
               { _gbName = "haskell"
               , _gbUpstream = Nothing
               }
  in GRepository
     { _grRemotes = []
     , _grBranches = [branch]
     , _grCurrentBranch = branch^.gbName
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
          , ReportInfo "Uncommitted modifications:"
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
        repo = defaultRepository & grStashes .~ [ GStash { _gsName = "WIP"
                                                         , _gsBranchName = "haskell"
                                                         }
                                                ]

      runGitActionPure repo ShowWork.cmd `shouldBe`
        ( repo
        , [ ReportInfo ">>> Branch refs:"
          , ReportInfo "local: haskell"
          , ReportInfo ""
          , ReportInfo ">>> Available stashes:"
          , PrintText "stash@{0}: WIP on haskell"
          ]
        )
