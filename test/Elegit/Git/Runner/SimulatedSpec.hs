module Elegit.Git.Runner.SimulatedSpec where

import Test.Hspec
import Universum

import Elegit.Git.Runner.Simulated

spec :: Spec
spec = do
  describe "git commit diff" $ do
    it "should show no difference" $ do
      let
        commit =
          GCommit
            { _gcName = "Initial"
            , _gcMessage = "Empty message"
            }
      commitDifference commit [commit] `shouldBe` []
    it "should show 1 commit difference" $ do
      let
        baseCommit =
          GCommit
            { _gcName = "Initial"
            , _gcMessage = "Empty message"
            }
        updateCommit =
          GCommit
            { _gcName = "Update"
            , _gcMessage = "Empty message"
            }
      commitDifference baseCommit [updateCommit, baseCommit] `shouldBe` [updateCommit]
