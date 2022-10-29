module Elegit.Cli.Action.AquireRepositorySpec where

import           Elegit.Cli.Action.AquireRepository
import           Elegit.Git.Runner.Simulated
import           Test.Hspec
import           Universum

spec :: Spec
spec = do
  describe "cmd" $ do
    it "executes expected list of commands" $ do
      let
        repoName = "git@github.com:bees-hive/elegant-git.git"

      collectImpureCommands (cmd repoName) `shouldBe`
        [ CloneRepositoryCommand repoName
        , UpdateConfigCommand "user.name" "username"
        , UpdateConfigCommand "user.email" "email"
        , UpdateConfigCommand "core.editor" "editor"
        , UpdateConfigCommand "core.commentChar" "|"
        , UpdateConfigCommand "apply.whitespace" "fix"
        , UpdateConfigCommand "fetch.prune" "true"
        , UpdateConfigCommand "fetch.pruneTags" "false"
        , UpdateConfigCommand "core.autoclrf" "input"
        , UpdateConfigCommand "pull.rebase" "true"
        , UpdateConfigCommand "rebase.autostash" "false"
        , UpdateConfigCommand "credential.helper" "osxkeychain"
        ]
