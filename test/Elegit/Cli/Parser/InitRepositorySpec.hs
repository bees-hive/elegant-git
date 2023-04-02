{-# LANGUAGE QuasiQuotes #-}
module Elegit.Cli.Parser.InitRepositorySpec where

import           Data.String.QQ
import           Elegit.Cli.Command     (ElegitCommand (..))
import           Elegit.Cli.Parser.Util as P
import           Test.Hspec
import           Universum


helpText :: Text
helpText = "Usage: git elegant init-repository \n" <> [s|

  Initializes a new repository and configures it.

Available options:
  -h,--help                Show this help text

Creates an empty Git repository (or reinitialize an existing one), runs its
configuration, and creates an initial empty commit.

Approximate commands flow is
```bash
==>> git elegant init-repository
git init
git elegant acquire-repository
git commit --allow-empty --file a-message-of-initial-commit
git show
```|]

invalidArgumentHelp :: Text
invalidArgumentHelp = [s|
Invalid argument `test-arg'

|] <> "Usage: git elegant init-repository \n" <> [s|

  Initializes a new repository and configures it.

Available options:
  -h,--help                Show this help text

Creates an empty Git repository (or reinitialize an existing one), runs its
configuration, and creates an initial empty commit.

Approximate commands flow is
```bash
==>> git elegant init-repository
git init
git elegant acquire-repository
git commit --allow-empty --file a-message-of-initial-commit
git show
```|]

invalidOptionHelp :: Text
invalidOptionHelp = [s|
Invalid option `--test-arg'

|] <> "Usage: git elegant init-repository \n" <> [s|

  Initializes a new repository and configures it.

Available options:
  -h,--help                Show this help text

Creates an empty Git repository (or reinitialize an existing one), runs its
configuration, and creates an initial empty commit.

Approximate commands flow is
```bash
==>> git elegant init-repository
git init
git elegant acquire-repository
git commit --allow-empty --file a-message-of-initial-commit
git show
```|]

spec :: Spec
spec = do
  describe "cmd" $ do
    it "parses command succefully" $ do
      parseMaybe "init-repository" [] `shouldBe` Just InitRepositoryCommand

    it "renders command help" $ do
      parseResult "init-repository" ["--help"] `shouldRender` helpText

    it "failes to parse unexpected arguments" $ do
      parseResult "init-repository" ["test-arg"] `shouldRender` invalidArgumentHelp

    it "failes to parse unexpected options" $ do
      parseResult "init-repository" ["--test-arg"] `shouldRender` invalidOptionHelp
