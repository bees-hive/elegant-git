module Elegit.Cli.Action.ShowWorkSpec where

import qualified Elegit.Cli.Action.ShowWork  as ShowWork
import           Elegit.Git.Runner.Simulated
import           Lens.Micro
import           Test.Hspec
import           Universum                   hiding (view, (%~), (.~), (^.))


defaultGit :: InMemoryGit
defaultGit =
  imGit
  where
    imGit = IMGit
      { _gConfig = mempty
      , _gRepository = pure repo
      }
    repo = GRepository
       { _grRemotes = []
       , _grBranches = [mainBranch, currentBranch]
       , _grCurrentBranch = currentBranch^.gbName
       , _grModifiedFiles = []
       , _grUnstagedFiles = []
       , _grStashes = [ ]
       , _grConfig = mempty
       }
    commit = GCommit
      { _gcName = "Init commit"
      , _gcMessage = "Empty message"
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

spec :: Spec
spec = do
  describe "cmd" $ do
    it "prints state of HEAD branch" $ do
      runGitActionPure defaultGit ShowWork.cmd `shouldBe`
        ( defaultGit
        , [ PrintText ">>> Branch refs:"
          , PrintText "local: haskell"
          , PrintText ""
          ]
        )
    it "print status when files changed" $ do
      let
        imGit = defaultGit &
            localRepository %~ \repo ->
              repo & grModifiedFiles .~ ["app/Main.hs"]
                   & grUnstagedFiles .~ ["tmp.txt"]
      runGitActionPure imGit ShowWork.cmd `shouldBe`
        ( imGit
        , [ PrintText ">>> Branch refs:"
          , PrintText "local: haskell"
          , PrintText ""
          , PrintText ">>> Uncommitted modifications:"
          , PrintText "M app/Main.hs"
          , PrintText "?? tmp.txt"
          ]
        )
    it "prints remote" $ do
      let
        imGit = defaultGit
          & localRepository.grBranches.each.filtered (\b -> b^.gbName == "haskell").gbUpstream ?~ "origin/haskell"
      runGitActionPure imGit ShowWork.cmd `shouldBe`
        ( imGit
        , [ PrintText ">>> Branch refs:"
          , PrintText "local: haskell"
          , PrintText "remote: origin/haskell"
          , PrintText ""
          ]
        )
    it "prints stash" $ do
      let
        stash = GStash
          { _gsName = "WIP"
          , _gsBranchName = "haskell"
          }
        imGit = defaultGit & localRepository.grStashes .~ [stash]

      runGitActionPure imGit ShowWork.cmd `shouldBe`
        ( imGit
        , [ PrintText ">>> Branch refs:"
          , PrintText "local: haskell"
          , PrintText ""
          , PrintText ">>> Available stashes:"
          , PrintText "stash@{0}: WIP on haskell"
          ]
        )
    it "prints log" $ do
      let
        newCommit =
          GCommit
            { _gcName = "Updates"
            , _gcMessage = "Empty message"
            }

        imGit = defaultGit & localRepository.branchWithName "haskell".gbCommit %~ (newCommit:)

      runGitActionPure imGit ShowWork.cmd `shouldBe`
        ( imGit
        , [ PrintText ">>> Branch refs:"
          , PrintText "local: haskell"
          , PrintText ""
          , PrintText ">>> New commits (comparing to main branch):"
          , PrintText "Updates"
          ]
        )
