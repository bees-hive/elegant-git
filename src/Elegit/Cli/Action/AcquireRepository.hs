{-# LANGUAGE QuasiQuotes #-}
module Elegit.Cli.Action.AcquireRepository
    ( cli
    , cmd
    ) where

import           Control.Monad.Free.Class
import           Data.String.QQ
import           Elegit.Cli.Command
import qualified Elegit.Git.Action               as GA
import           Fmt
import           Options.Applicative
import qualified Options.Applicative.Help.Pretty as OA
import           Universum


site:: Text
site = "placeholder"

purpose :: OA.Doc
purpose = OA.text "Configures the current local Git repository."

description :: OA.Doc
description = OA.string $ [s|
Applies the "basics", "standards", "aliases", and "signature" configurations
to the current Git repository using `git config --local`. The command asks to
provide information that is needed for the current repository configuration.

The behavior of the command varies depend on `git elegant acquire-git`
execution (a global configuration). If the global configuration is applied,
then this command configures repository-related staffs only, otherwise, it
applies all configurations to the current local repository.

To find out what will be configured, please visit
|] ++ (fmt ""+|site|+"/en/latest/configuration/")


cli :: Mod CommandFields ElegitCommand
cli = command "acquire-repository" $ info (pure AcquireRepositoryCommand) $
    mconcat [ progDescDoc (Just purpose )
            , footerDoc (Just description )
            ]


data ConfigKey
  = UserNameKey
  | UserEmailKey
  | CoreEditorKey
  | DefaultBranchKey
  | ProtectedBranchesKey


configName :: ConfigKey -> Text
configName UserNameKey          = "user.name"
configName UserEmailKey         = "user.email"
configName CoreEditorKey        = "core.editor"
configName DefaultBranchKey     = "elegant-git.default-branch"
configName ProtectedBranchesKey = "elegant-git.protected-branches"


configPrompt :: ConfigKey -> Text
configPrompt UserNameKey          = "What is your user name?"
configPrompt UserEmailKey         = "What is your email?"
configPrompt CoreEditorKey        = "What is the command to launching an editor?"
configPrompt DefaultBranchKey     = "What is the default branch?"
configPrompt ProtectedBranchesKey = "What are protected branches (split with space)"


configDefault :: (MonadFree GA.GitF m) => ConfigKey -> m (Maybe Text)
configDefault cKey = case cKey of
    UserNameKey          -> getFromConfig
    UserEmailKey         -> getFromConfig
    CoreEditorKey        -> getFromConfig
    DefaultBranchKey     -> return $ Just "master"
    ProtectedBranchesKey -> return $ Just "master"

    where
        getFromConfig :: (MonadFree GA.GitF m) => m (Maybe Text)
        getFromConfig = GA.readConfig GA.AutoConfig (configName cKey)


configureBasics :: (MonadFree GA.GitF m) => GA.ConfigScope -> m ()
configureBasics cScope = do
    for_ basicConfigs $ \cKey -> do
        keyDefault <- configDefault cKey
        newValue <- GA.promptDefault (configPrompt cKey) keyDefault
        GA.setConfigVerbose cScope (configName cKey) newValue

    where
        basicConfigs :: [ConfigKey]
        basicConfigs =
            [ UserNameKey
            , UserEmailKey
            , CoreEditorKey
            , DefaultBranchKey
            , ProtectedBranchesKey
            ]


configureStandards :: (MonadFree GA.GitF m) => GA.ConfigScope -> m ()
configureStandards cScope =
    for_ standardConfigs $ \(cKey,cValue) -> do
        GA.setConfigVerbose cScope cKey cValue
    where
        standardConfigs :: [(Text, Text)]
        standardConfigs =
            [ ("core.commentChar", "|")
            , ("apply.whitespace", "fix")
            , ("fetch.prune", "true")
            , ("fetch.pruneTags", "false")
            , ("core.autocrlf", "input")
            , ("pull.rebase", "true")
            , ("rebase.autoStash", "false")
            , ("credential.helper", "osxkeychain")
            ]


configureAliases :: (MonadFree GA.GitF m) => GA.ConfigScope -> m ()
configureAliases cScope = do
    forM_ elegantCommands $ \elegantCommand -> do
        let
            alias = "alias."+|elegantCommand|+""
            origin = "elegant "+|elegantCommand|+""
        GA.setConfigVerbose cScope alias origin
    where
        -- TODO: Think if there is a way to make it centralised
        elegantCommands :: [Text]
        elegantCommands =
            [ "accept-work"
            , "acquire-git"
            , "acquire-repository"
            , "actualize-work"
            , "amend-work"
            , "clone-repository"
            , "deliver-work"
            , "init-repository"
            , "make-workflow"
            , "obtain-work"
            , "polish-work"
            , "polish-workflow"
            , "prune-repository"
            , "release-work"
            , "save-work"
            , "show-commands"
            , "show-release-notes"
            , "show-work"
            , "show-workflows"
            , "start-work"
            ]


-- TODO: port bash logic
setupGPGSignature :: (MonadFree GA.GitF m) => m ()
setupGPGSignature = pass


-- | Execution description of the AcquireRepository action
cmd :: (MonadFree GA.GitF m) => m ()
cmd = do
    GA.removeObsoleteConfiguration GA.LocalConfig
    GA.print =<< GA.formatInfoBox "Configuring basics..."
    configureBasics GA.LocalConfig
    unlessM GA.isGitAcquired $ do
        GA.print =<< GA.formatInfoBox "Configuring standards..."
        configureStandards GA.LocalConfig
        GA.print =<< GA.formatInfoBox "Configuring aliases..."
        configureAliases GA.LocalConfig
    GA.print =<< GA.formatInfoBox "Configuring signature..."
    setupGPGSignature
