{-# LANGUAGE QuasiQuotes #-}
module Elegit.Cli.Parser.ShowWorkSpec where

import           Data.String.QQ
import           Elegit.Cli.Command     (ElegitCommand (..))
import           Elegit.Cli.Parser.Util as P
import           Test.Hspec
import           Universum


helpText :: Text
helpText = "Usage: git elegant show-work \n" <> [s|

  Prints HEAD state.

Available options:
  -h,--help                Show this help text

Prints HEAD state by displaying local and remote-tracking (if available) refs,
commits that aren't in the default development branch, uncommitted
modifications, and available stashes.

Approximate commands flow is
```bash
==>> git elegant show-work
git log --oneline master..@
git status --short
git stash list
```|]

invalidArgumentHelp :: Text
invalidArgumentHelp = [s|
Invalid argument `test-arg'

|] <> "Usage: git elegant show-work \n" <> [s|

  Prints HEAD state.

Available options:
  -h,--help                Show this help text

Prints HEAD state by displaying local and remote-tracking (if available) refs,
commits that aren't in the default development branch, uncommitted
modifications, and available stashes.

Approximate commands flow is
```bash
==>> git elegant show-work
git log --oneline master..@
git status --short
git stash list
```|]

invalidOptionHelp :: Text
invalidOptionHelp = [s|
Invalid option `--test-arg'

|] <> "Usage: git elegant show-work \n" <> [s|

  Prints HEAD state.

Available options:
  -h,--help                Show this help text

Prints HEAD state by displaying local and remote-tracking (if available) refs,
commits that aren't in the default development branch, uncommitted
modifications, and available stashes.

Approximate commands flow is
```bash
==>> git elegant show-work
git log --oneline master..@
git status --short
git stash list
```|]

spec :: Spec
spec = do
  describe "cmd" $ do
    it "parses command succefully" $ do
      parseMaybe "show-work" [] `shouldBe` Just ShowWorkCommand

    it "renders command help" $ do
      parseResult "show-work" ["--help"] `shouldRender` helpText

    it "failes to parse unexpected arguments" $ do
      parseResult "show-work" ["test-arg"] `shouldRender` invalidArgumentHelp

    it "failes to parse unexpected options" $ do
      parseResult "show-work" ["--test-arg"] `shouldRender` invalidOptionHelp
