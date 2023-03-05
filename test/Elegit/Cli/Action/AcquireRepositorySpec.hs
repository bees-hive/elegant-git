{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes     #-}
module Elegit.Cli.Action.AcquireRepositorySpec where

import           Data.HashMap.Strict                 (delete, union)
import           Data.String.QQ
import qualified Elegit.Cli.Action.AcquireRepository as AcquireRepository
import           Elegit.Git.Runner.Simulated
import           Lens.Micro
import           Test.Hspec
import           Universum                           hiding (view, (%~), (.~), (^.))


defaultGit :: InMemoryGit
defaultGit =
  imGit
  where
    imGit = IMGit
      { _gConfig = mempty
      , _gRepository = repo
      }
    repo = GRepository
       { _grRemotes = []
       , _grBranches = [mainBranch, currentBranch]
       , _grCurrentBranch = currentBranch^.gbName
       , _grModifiedFiles = []
       , _grUnstagedFiles = []
       , _grStashes = []
       , _grConfig = []
       }
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

standardsOutputBlock :: [GitCommand]
standardsOutputBlock =
  [ PrintText [s|==============================|]
  , PrintText [s|== Configuring standards... ==|]
  , PrintText [s|==============================|]
  , PrintText [s|==>> git config --local core.commentChar "|"|]
  , PrintText [s|==>> git config --local apply.whitespace "fix"|]
  , PrintText [s|==>> git config --local fetch.prune "true"|]
  , PrintText [s|==>> git config --local fetch.pruneTags "false"|]
  , PrintText [s|==>> git config --local core.autocrlf "input"|]
  , PrintText [s|==>> git config --local pull.rebase "true"|]
  , PrintText [s|==>> git config --local rebase.autoStash "false"|]
  , PrintText [s|==>> git config --local credential.helper "osxkeychain"|]
  ]

aliasesOutputBlock :: [GitCommand]
aliasesOutputBlock =
  [ PrintText [s|============================|]
  , PrintText [s|== Configuring aliases... ==|]
  , PrintText [s|============================|]
  , PrintText [s|==>> git config --local alias.accept-work "elegant accept-work"|]
  , PrintText [s|==>> git config --local alias.acquire-git "elegant acquire-git"|]
  , PrintText [s|==>> git config --local alias.acquire-repository "elegant acquire-repository"|]
  , PrintText [s|==>> git config --local alias.actualize-work "elegant actualize-work"|]
  , PrintText [s|==>> git config --local alias.amend-work "elegant amend-work"|]
  , PrintText [s|==>> git config --local alias.clone-repository "elegant clone-repository"|]
  , PrintText [s|==>> git config --local alias.deliver-work "elegant deliver-work"|]
  , PrintText [s|==>> git config --local alias.init-repository "elegant init-repository"|]
  , PrintText [s|==>> git config --local alias.make-workflow "elegant make-workflow"|]
  , PrintText [s|==>> git config --local alias.obtain-work "elegant obtain-work"|]
  , PrintText [s|==>> git config --local alias.polish-work "elegant polish-work"|]
  , PrintText [s|==>> git config --local alias.polish-workflow "elegant polish-workflow"|]
  , PrintText [s|==>> git config --local alias.prune-repository "elegant prune-repository"|]
  , PrintText [s|==>> git config --local alias.release-work "elegant release-work"|]
  , PrintText [s|==>> git config --local alias.save-work "elegant save-work"|]
  , PrintText [s|==>> git config --local alias.show-commands "elegant show-commands"|]
  , PrintText [s|==>> git config --local alias.show-release-notes "elegant show-release-notes"|]
  , PrintText [s|==>> git config --local alias.show-work "elegant show-work"|]
  , PrintText [s|==>> git config --local alias.show-workflows "elegant show-workflows"|]
  , PrintText [s|==>> git config --local alias.start-work "elegant start-work"|]
  ]

signatureOutputBlock :: [GitCommand]
signatureOutputBlock =
  [ PrintText "=============================="
  , PrintText "== Configuring signature... =="
  , PrintText "=============================="
  ]

configuredStandards :: HashMap Text Text
configuredStandards =
  [ ("core.editor",                    "test")
  , ("apply.whitespace",               "fix")
  , ("fetch.pruneTags",                "false")
  , ("core.autocrlf",                  "input")
  , ("fetch.prune",                    "true")
  , ("elegant-git.default-branch",     "test")
  , ("user.email",                     "test")
  , ("user.name",                      "test")
  , ("pull.rebase",                    "true")
  , ("rebase.autoStash",               "false")
  , ("core.commentChar",               "|")
  , ("credential.helper",              "osxkeychain")
  , ("elegant-git.protected-branches", "test")
  ]

configuredAliases :: HashMap Text Text
configuredAliases =
  [ ("alias.accept-work",        "elegant accept-work")
  , ("alias.acquire-git",        "elegant acquire-git")
  , ("alias.acquire-repository", "elegant acquire-repository")
  , ("alias.actualize-work",     "elegant actualize-work")
  , ("alias.amend-work",         "elegant amend-work")
  , ("alias.clone-repository",   "elegant clone-repository")
  , ("alias.deliver-work",       "elegant deliver-work")
  , ("alias.init-repository",    "elegant init-repository")
  , ("alias.make-workflow",      "elegant make-workflow")
  , ("alias.obtain-work",        "elegant obtain-work")
  , ("alias.polish-work",        "elegant polish-work")
  , ("alias.polish-workflow",    "elegant polish-workflow")
  , ("alias.prune-repository",   "elegant prune-repository")
  , ("alias.release-work",       "elegant release-work")
  , ("alias.save-work",          "elegant save-work")
  , ("alias.show-commands",      "elegant show-commands")
  , ("alias.show-release-notes", "elegant show-release-notes")
  , ("alias.show-work",          "elegant show-work")
  , ("alias.show-workflows",     "elegant show-workflows")
  , ("alias.start-work",         "elegant start-work")
  ]

spec :: Spec
spec = do
  describe "cmd" $ do
    it "prepares local git repository for further work" $ do
      let
        repoWithNewConfig = defaultGit
          & gRepository.grConfig %~ union
            (configuredStandards `union` configuredAliases)

      runGitActionPure defaultGit AcquireRepository.cmd `shouldBe`
        ( repoWithNewConfig
        , [ PrintText [s|=========================================|]
          , PrintText [s|== Removing obsolete configurations... ==|]
          , PrintText [s|=========================================|]
          , PrintText [s|===========================|]
          , PrintText [s|== Configuring basics... ==|]
          , PrintText [s|===========================|]
          , Prompt    [s|What is your user name?: test|]
          , PrintText [s|==>> git config --local user.name "test"|]
          , Prompt    [s|What is your email?: test|]
          , PrintText [s|==>> git config --local user.email "test"|]
          , Prompt    [s|What is the command to launching an editor?: test|]
          , PrintText [s|==>> git config --local core.editor "test"|]
          , Prompt    [s|What is the default branch? {master}: test|]
          , PrintText [s|==>> git config --local elegant-git.default-branch "test"|]
          , Prompt    [s|What are protected branches (split with space) {master}: test|]
          , PrintText [s|==>> git config --local elegant-git.protected-branches "test"|]
          ]
         ++ standardsOutputBlock
         ++ aliasesOutputBlock
         ++ signatureOutputBlock
        )

    it "uses global config values as defaults for prompts" $ do
      let
        git = defaultGit
          & gConfig %~ union
            [ ("core.editor", "test-editor")
            , ("user.email",  "test-email")
            , ("user.name",   "test-name")
            ]

        repoWithNewConfig = git
          & gRepository.grConfig %~ union
            (configuredStandards `union` configuredAliases)

      runGitActionPure git AcquireRepository.cmd `shouldBe`
        ( repoWithNewConfig
        , [ PrintText [s|=========================================|]
          , PrintText [s|== Removing obsolete configurations... ==|]
          , PrintText [s|=========================================|]
          , PrintText [s|===========================|]
          , PrintText [s|== Configuring basics... ==|]
          , PrintText [s|===========================|]
          , Prompt    [s|What is your user name? {test-name}: test|]
          , PrintText [s|==>> git config --local user.name "test"|]
          , Prompt    [s|What is your email? {test-email}: test|]
          , PrintText [s|==>> git config --local user.email "test"|]
          , Prompt    [s|What is the command to launching an editor? {test-editor}: test|]
          , PrintText [s|==>> git config --local core.editor "test"|]
          , Prompt    [s|What is the default branch? {master}: test|]
          , PrintText [s|==>> git config --local elegant-git.default-branch "test"|]
          , Prompt    [s|What are protected branches (split with space) {master}: test|]
          , PrintText [s|==>> git config --local elegant-git.protected-branches "test"|]
          ]
         ++ standardsOutputBlock
         ++ aliasesOutputBlock
         ++ signatureOutputBlock
        )

    it "does not alter unexpected config" $ do
      let
        git = defaultGit
          & gRepository.grConfig %~ union
            [ ("test.test",  "test")
            ]

        repoWithNewConfig = git
          & gRepository.grConfig %~ union
            (configuredStandards `union` configuredAliases)

      runGitActionPure git AcquireRepository.cmd `shouldBe`
        ( repoWithNewConfig
        , [ PrintText [s|=========================================|]
          , PrintText [s|== Removing obsolete configurations... ==|]
          , PrintText [s|=========================================|]
          , PrintText [s|===========================|]
          , PrintText [s|== Configuring basics... ==|]
          , PrintText [s|===========================|]
          , Prompt    [s|What is your user name?: test|]
          , PrintText [s|==>> git config --local user.name "test"|]
          , Prompt    [s|What is your email?: test|]
          , PrintText [s|==>> git config --local user.email "test"|]
          , Prompt    [s|What is the command to launching an editor?: test|]
          , PrintText [s|==>> git config --local core.editor "test"|]
          , Prompt    [s|What is the default branch? {master}: test|]
          , PrintText [s|==>> git config --local elegant-git.default-branch "test"|]
          , Prompt    [s|What are protected branches (split with space) {master}: test|]
          , PrintText [s|==>> git config --local elegant-git.protected-branches "test"|]
          ]
         ++ standardsOutputBlock
         ++ aliasesOutputBlock
         ++ signatureOutputBlock
        )

    it "removes obsolete configuration" $ do
      let
        git = defaultGit
          & gRepository.grConfig %~ union
            [ ("elegant.acquired",  "true")
            ]

        repoWithNewConfig = git
          & gRepository.grConfig %~
            delete "elegant.acquired"
          & gRepository.grConfig %~ union
            (configuredStandards `union` configuredAliases)

      runGitActionPure git AcquireRepository.cmd `shouldBe`
        ( repoWithNewConfig
        , [ PrintText [s|=========================================|]
          , PrintText [s|== Removing obsolete configurations... ==|]
          , PrintText [s|=========================================|]
          , PrintText [s|Removing old Elegnat Git configuration keys...|]
          , PrintText [s|==>> git config --local --unset elegant.acquired|]
          , PrintText [s|===========================|]
          , PrintText [s|== Configuring basics... ==|]
          , PrintText [s|===========================|]
          , Prompt    [s|What is your user name?: test|]
          , PrintText [s|==>> git config --local user.name "test"|]
          , Prompt    [s|What is your email?: test|]
          , PrintText [s|==>> git config --local user.email "test"|]
          , Prompt    [s|What is the command to launching an editor?: test|]
          , PrintText [s|==>> git config --local core.editor "test"|]
          , Prompt    [s|What is the default branch? {master}: test|]
          , PrintText [s|==>> git config --local elegant-git.default-branch "test"|]
          , Prompt    [s|What are protected branches (split with space) {master}: test|]
          , PrintText [s|==>> git config --local elegant-git.protected-branches "test"|]
          ]
         ++ standardsOutputBlock
         ++ aliasesOutputBlock
         ++ signatureOutputBlock
        )

    it "removes old aliases" $ do
      let
        git = defaultGit
          & gRepository.grConfig %~ union
          [ ("alias.polish-work",        "elegant polish-work")
          , ("alias.polish-workflow",    "elegant polish-workflow")
          , ("alias.prune-repository",   "elegant prune-repository")
          ]

        repoWithNewConfig = git
          & gRepository.grConfig %~ union
            (configuredStandards `union` configuredAliases)

      runGitActionPure git AcquireRepository.cmd `shouldBe`
        ( repoWithNewConfig
        , [ PrintText [s|=========================================|]
          , PrintText [s|== Removing obsolete configurations... ==|]
          , PrintText [s|=========================================|]
          , PrintText [s|Removing old Elegant Git aliases...|]
          , PrintText [s|==>> git config --local --unset alias.polish-work|]
          , PrintText [s|==>> git config --local --unset alias.polish-workflow|]
          , PrintText [s|==>> git config --local --unset alias.prune-repository|]
          , PrintText [s|===========================|]
          , PrintText [s|== Configuring basics... ==|]
          , PrintText [s|===========================|]
          , Prompt    [s|What is your user name?: test|]
          , PrintText [s|==>> git config --local user.name "test"|]
          , Prompt    [s|What is your email?: test|]
          , PrintText [s|==>> git config --local user.email "test"|]
          , Prompt    [s|What is the command to launching an editor?: test|]
          , PrintText [s|==>> git config --local core.editor "test"|]
          , Prompt    [s|What is the default branch? {master}: test|]
          , PrintText [s|==>> git config --local elegant-git.default-branch "test"|]
          , Prompt    [s|What are protected branches (split with space) {master}: test|]
          , PrintText [s|==>> git config --local elegant-git.protected-branches "test"|]
          ]
         ++ standardsOutputBlock
         ++ aliasesOutputBlock
         ++ signatureOutputBlock
        )
