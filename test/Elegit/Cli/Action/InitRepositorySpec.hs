{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}

module Elegit.Cli.Action.InitRepositorySpec where

import Data.HashMap.Strict (union)
import Data.String.QQ
import qualified Elegit.Cli.Action.InitRepository as InitRepository
import Elegit.Git.Runner.Simulated
import Lens.Micro
import Test.Hspec
import Universum hiding (view, (%~), (.~), (^.))

defaultGit :: InMemoryGit
defaultGit =
  imGit
 where
  imGit =
    IMGit
      { _gConfig = mempty
      , _gRepository = Nothing
      }

standardsOutputBlock :: [GitCommand]
standardsOutputBlock =
  [ PrintText [s|==============================|]
  , PrintText [s|== Configuring standards... ==|]
  , PrintText [s|==============================|]
  , PrintText [s|==>> git config --local core.commentChar ||]
  , PrintText [s|==>> git config --local apply.whitespace fix|]
  , PrintText [s|==>> git config --local fetch.prune true|]
  , PrintText [s|==>> git config --local fetch.pruneTags false|]
  , PrintText [s|==>> git config --local core.autocrlf input|]
  , PrintText [s|==>> git config --local pull.rebase true|]
  , PrintText [s|==>> git config --local rebase.autoStash false|]
  , PrintText [s|==>> git config --local credential.helper osxkeychain|]
  ]

aliasesOutputBlock :: [GitCommand]
aliasesOutputBlock =
  [ PrintText [s|============================|]
  , PrintText [s|== Configuring aliases... ==|]
  , PrintText [s|============================|]
  , PrintText [s|==>> git config --local alias.accept-work elegant accept-work|]
  , PrintText [s|==>> git config --local alias.acquire-git elegant acquire-git|]
  , PrintText [s|==>> git config --local alias.acquire-repository elegant acquire-repository|]
  , PrintText [s|==>> git config --local alias.actualize-work elegant actualize-work|]
  , PrintText [s|==>> git config --local alias.amend-work elegant amend-work|]
  , PrintText [s|==>> git config --local alias.clone-repository elegant clone-repository|]
  , PrintText [s|==>> git config --local alias.deliver-work elegant deliver-work|]
  , PrintText [s|==>> git config --local alias.init-repository elegant init-repository|]
  , PrintText [s|==>> git config --local alias.make-workflow elegant make-workflow|]
  , PrintText [s|==>> git config --local alias.obtain-work elegant obtain-work|]
  , PrintText [s|==>> git config --local alias.polish-work elegant polish-work|]
  , PrintText [s|==>> git config --local alias.polish-workflow elegant polish-workflow|]
  , PrintText [s|==>> git config --local alias.prune-repository elegant prune-repository|]
  , PrintText [s|==>> git config --local alias.release-work elegant release-work|]
  , PrintText [s|==>> git config --local alias.save-work elegant save-work|]
  , PrintText [s|==>> git config --local alias.show-commands elegant show-commands|]
  , PrintText [s|==>> git config --local alias.show-release-notes elegant show-release-notes|]
  , PrintText [s|==>> git config --local alias.show-work elegant show-work|]
  , PrintText [s|==>> git config --local alias.show-workflows elegant show-workflows|]
  , PrintText [s|==>> git config --local alias.start-work elegant start-work|]
  ]

signatureOutputBlock :: [GitCommand]
signatureOutputBlock =
  [ PrintText [s|==============================|]
  , PrintText [s|== Configuring signature... ==|]
  , PrintText [s|==============================|]
  , PrintText [s|==>> gpg --list-secret-keys --keyid-format long test|]
  , PrintText [s|3AA5C34371567BD2|]
  , PrintText [s||]
  , PrintText [s|From the list of GPG keys above, copy the GPG key ID you'd like to use.|]
  , PrintText [s|It will be|]
  , PrintText [s|    3AA5C34371567BD2|]
  , PrintText [s|for the output like this|]
  , PrintText [s|    sec   4096R/3AA5C34371567BD2 2016-03-10 [expires: 2017-03-10]|]
  , PrintText [s|    A330C91F8EC4BC7AECFA63E03AA5C34371567BD2|]
  , PrintText [s|    uid                          Hubot|]
  , PrintText [s||]
  , PrintText [s|If you don't want to configure signature, just hit Enter button.|]
  , Prompt [s|Please pass a key that has to sign objects of the current repository: test|]
  , PrintText [s|==>> git config --local user.signingkey test|]
  , PrintText [s|==>> git config --local gpg.program /usr/bin/gpg|]
  , PrintText [s|==>> git config --local commit.gpgsign true|]
  , PrintText [s|==>> git config --local tag.forceSignAnnotated true|]
  , PrintText [s|==>> git config --local tag.gpgSign true|]
  ]

configuredStandards :: HashMap Text Text
configuredStandards =
  [ ("core.editor", "test")
  , ("apply.whitespace", "fix")
  , ("fetch.pruneTags", "false")
  , ("core.autocrlf", "input")
  , ("fetch.prune", "true")
  , ("elegant-git.default-branch", "test")
  , ("user.email", "test")
  , ("user.name", "test")
  , ("pull.rebase", "true")
  , ("rebase.autoStash", "false")
  , ("core.commentChar", "|")
  , ("credential.helper", "osxkeychain")
  , ("elegant-git.protected-branches", "test")
  ]

configuredAliases :: HashMap Text Text
configuredAliases =
  [ ("alias.accept-work", "elegant accept-work")
  , ("alias.acquire-git", "elegant acquire-git")
  , ("alias.acquire-repository", "elegant acquire-repository")
  , ("alias.actualize-work", "elegant actualize-work")
  , ("alias.amend-work", "elegant amend-work")
  , ("alias.clone-repository", "elegant clone-repository")
  , ("alias.deliver-work", "elegant deliver-work")
  , ("alias.init-repository", "elegant init-repository")
  , ("alias.make-workflow", "elegant make-workflow")
  , ("alias.obtain-work", "elegant obtain-work")
  , ("alias.polish-work", "elegant polish-work")
  , ("alias.polish-workflow", "elegant polish-workflow")
  , ("alias.prune-repository", "elegant prune-repository")
  , ("alias.release-work", "elegant release-work")
  , ("alias.save-work", "elegant save-work")
  , ("alias.show-commands", "elegant show-commands")
  , ("alias.show-release-notes", "elegant show-release-notes")
  , ("alias.show-work", "elegant show-work")
  , ("alias.show-workflows", "elegant show-workflows")
  , ("alias.start-work", "elegant start-work")
  ]

configuredGpg :: HashMap Text Text
configuredGpg =
  [ ("user.signingkey", "test")
  , ("tag.gpgSign", "true")
  , ("gpg.program", "/usr/bin/gpg")
  , ("commit.gpgsign", "true")
  , ("tag.forceSignAnnotated", "true")
  ]

spec :: Spec
spec = do
  describe "cmd" $ do
    it "initializes new repository with standard configuration" $ do
      let
        commit =
          GCommit
            { _gcName = "Initial Commit"
            , _gcMessage = "Add initial empty commit\n\nThis commit is the first commit in this working tree. It does not have\nany changes. However, it simplifies further work at least in the\nfollowing cases:\n- it's possible to create a branch now\n- it's possible to manage the second commit if it requires some\npolishing after creation\n\nThis commit is created automatically by Elegant Git after the\ninitialization of a new repository."
            }
        repository =
          GRepository
            { _grRemotes = []
            , _grBranches =
                [ GBranch
                    { _gbName = "master"
                    , _gbUpstream = Nothing
                    , _gbCommit = [commit]
                    }
                ]
            , _grCurrentBranch = "master"
            , _grStashes = []
            , _grModifiedFiles = []
            , _grUnstagedFiles = []
            , _grConfig = configuredStandards `union` configuredAliases `union` configuredGpg
            }
        repoWithNewConfig =
          defaultGit & gRepository ?~ repository

      runGitActionPure defaultGit InitRepository.cmd
        `shouldBe` ( repoWithNewConfig
                   , [ PrintText [s|==>> git init|]
                     , PrintText [s|=========================================|]
                     , PrintText [s|== Removing obsolete configurations... ==|]
                     , PrintText [s|=========================================|]
                     , PrintText [s|===========================|]
                     , PrintText [s|== Configuring basics... ==|]
                     , PrintText [s|===========================|]
                     , Prompt [s|What is your user name?: test|]
                     , PrintText [s|==>> git config --local user.name test|]
                     , Prompt [s|What is your email?: test|]
                     , PrintText [s|==>> git config --local user.email test|]
                     , Prompt [s|What is the command to launching an editor?: test|]
                     , PrintText [s|==>> git config --local core.editor test|]
                     , Prompt [s|What is the default branch? {master}: test|]
                     , PrintText [s|==>> git config --local elegant-git.default-branch test|]
                     , Prompt [s|What are protected branches (split with space) {master}: test|]
                     , PrintText [s|==>> git config --local elegant-git.protected-branches test|]
                     ]
                      ++ standardsOutputBlock
                      ++ aliasesOutputBlock
                      ++ signatureOutputBlock
                      ++ [ PrintText [s|==>> git commit --allow-empty --file a-message-of-initial-commit|]
                         , PrintText [s|==>> git show HEAD|]
                         , PrintText [s|Initial Commit|]
                         , PrintText ""
                         , PrintText [s|Add initial empty commit|]
                         , PrintText ""
                         , PrintText [s|This commit is the first commit in this working tree. It does not have|]
                         , PrintText [s|any changes. However, it simplifies further work at least in the|]
                         , PrintText [s|following cases:|]
                         , PrintText [s|- it's possible to create a branch now|]
                         , PrintText [s|- it's possible to manage the second commit if it requires some|]
                         , PrintText [s|polishing after creation|]
                         , PrintText ""
                         , PrintText [s|This commit is created automatically by Elegant Git after the|]
                         , PrintText [s|initialization of a new repository.|]
                         ]
                   )
