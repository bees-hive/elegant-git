{-# LANGUAGE QuasiQuotes #-}

module Elegit.Cli.Parser.AcquireRepositorySpec where

import Data.String.QQ
import Elegit.Cli.Command (ElegitCommand (..))
import Elegit.Cli.Parser.Util as P
import Test.Hspec
import Universum

helpText :: Text
helpText =
  "Usage: git elegant acquire-repository \n"
    <> [s|

  Configures the current local Git repository.

Available options:
  -h,--help                Show this help text

Applies the "basics", "standards", "aliases", and "signature" configurations
to the current Git repository using `git config --local`. The command asks to
provide information that is needed for the current repository configuration.

The behavior of the command varies depend on `git elegant acquire-git`
execution (a global configuration). If the global configuration is applied,
then this command configures repository-related staffs only, otherwise, it
applies all configurations to the current local repository.

To find out what will be configured, please visit
placeholder/en/latest/configuration/|]

invalidArgumentHelp :: Text
invalidArgumentHelp =
  [s|
Invalid argument `test-arg'

|]
    <> "Usage: git elegant acquire-repository \n"
    <> [s|

  Configures the current local Git repository.

Available options:
  -h,--help                Show this help text

Applies the "basics", "standards", "aliases", and "signature" configurations
to the current Git repository using `git config --local`. The command asks to
provide information that is needed for the current repository configuration.

The behavior of the command varies depend on `git elegant acquire-git`
execution (a global configuration). If the global configuration is applied,
then this command configures repository-related staffs only, otherwise, it
applies all configurations to the current local repository.

To find out what will be configured, please visit
placeholder/en/latest/configuration/|]

invalidOptionHelp :: Text
invalidOptionHelp =
  [s|
Invalid option `--test-arg'

|]
    <> "Usage: git elegant acquire-repository \n"
    <> [s|

  Configures the current local Git repository.

Available options:
  -h,--help                Show this help text

Applies the "basics", "standards", "aliases", and "signature" configurations
to the current Git repository using `git config --local`. The command asks to
provide information that is needed for the current repository configuration.

The behavior of the command varies depend on `git elegant acquire-git`
execution (a global configuration). If the global configuration is applied,
then this command configures repository-related staffs only, otherwise, it
applies all configurations to the current local repository.

To find out what will be configured, please visit
placeholder/en/latest/configuration/|]

spec :: Spec
spec = do
  describe "cmd" $ do
    it "parses command succefully" $ do
      parseMaybe "acquire-repository" [] `shouldBe` Just AcquireRepositoryCommand

    it "renders command help" $ do
      parseResult "acquire-repository" ["--help"] `shouldRender` helpText

    it "failes to parse unexpected arguments" $ do
      parseResult "acquire-repository" ["test-arg"] `shouldRender` invalidArgumentHelp

    it "failes to parse unexpected options" $ do
      parseResult "acquire-repository" ["--test-arg"] `shouldRender` invalidOptionHelp
