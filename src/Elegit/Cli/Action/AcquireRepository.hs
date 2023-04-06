{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}

module Elegit.Cli.Action.AcquireRepository
  ( cli
  , cmd
  ) where

import Control.Monad.Free.Class
import Data.String.QQ
import Elegit.Cli.Command
import qualified Elegit.Git.Action as GA
import Fmt
import Options.Applicative
import qualified Options.Applicative.Help.Pretty as OA
import Universum

site :: Text
site = "placeholder"

purpose :: OA.Doc
purpose = OA.text "Configures the current local Git repository."

description :: OA.Doc
description =
  OA.string $
    [s|
Applies the "basics", "standards", "aliases", and "signature" configurations
to the current Git repository using `git config --local`. The command asks to
provide information that is needed for the current repository configuration.

The behavior of the command varies depend on `git elegant acquire-git`
execution (a global configuration). If the global configuration is applied,
then this command configures repository-related staffs only, otherwise, it
applies all configurations to the current local repository.

To find out what will be configured, please visit
|]
      ++ (fmt "" +| site |+ "/en/latest/configuration/")

cli :: Mod CommandFields ElegitCommand
cli =
  command "acquire-repository" $
    info (pure AcquireRepositoryCommand) $
      mconcat
        [ progDescDoc (Just purpose)
        , footerDoc (Just description)
        ]

data ConfigKey
  = UserNameKey
  | UserEmailKey
  | CoreEditorKey
  | DefaultBranchKey
  | ProtectedBranchesKey

configName :: ConfigKey -> Text
configName UserNameKey = "user.name"
configName UserEmailKey = "user.email"
configName CoreEditorKey = "core.editor"
configName DefaultBranchKey = "elegant-git.default-branch"
configName ProtectedBranchesKey = "elegant-git.protected-branches"

configPrompt :: ConfigKey -> Text
configPrompt UserNameKey = "What is your user name?"
configPrompt UserEmailKey = "What is your email?"
configPrompt CoreEditorKey = "What is the command to launching an editor?"
configPrompt DefaultBranchKey = "What is the default branch?"
configPrompt ProtectedBranchesKey = "What are protected branches (split with space)"

configDefault :: (MonadFree GA.GitF m) => ConfigKey -> m (Maybe Text)
configDefault cKey = case cKey of
  UserNameKey -> getFromConfig
  UserEmailKey -> getFromConfig
  CoreEditorKey -> getFromConfig
  DefaultBranchKey -> return $ Just "master"
  ProtectedBranchesKey -> return $ Just "master"
 where
  getFromConfig :: (MonadFree GA.GitF m) => m (Maybe Text)
  getFromConfig = GA.readConfig GA.AutoConfig (configName cKey)

configureBasics :: (MonadFree GA.GitF m) => GA.ConfigScope -> m ()
configureBasics cScope = do
  for_ basicConfigs $ \cKey -> do
    mKeyDefault <- configDefault cKey
    newValue <- GA.promptDefault (configPrompt cKey) mKeyDefault
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
  for_ standardConfigs $ \(cKey, cValue) -> do
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
      alias = "alias." +| elegantCommand |+ ""
      origin = "elegant " +| elegantCommand |+ ""
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

setupGPGSignature :: (MonadFree GA.GitF m) => m ()
setupGPGSignature = void $ runMaybeT $ do
  userEmail <- MaybeT $ GA.readConfig GA.LocalConfig (configName UserEmailKey)
  pathToGPG <- MaybeT $ GA.pathToTool "gpg"
  GA.gpgListKeysVerbose userEmail >>= \case
    Nothing -> do
      GA.print =<< GA.formatInfo "There is no gpg key for the given email."
      GA.print =<< GA.formatInfo "A signature is not configured."
    Just gpgKeysOutput -> do
      mapM_ GA.print gpgKeysOutput
      GA.print ""
      GA.print =<< GA.formatInfo "From the list of GPG keys above, copy the GPG key ID you'd like to use."
      GA.print =<< GA.formatInfo "It will be"
      GA.print =<< GA.formatInfo "    3AA5C34371567BD2"
      GA.print =<< GA.formatInfo "for the output like this"
      GA.print =<< GA.formatInfo "    sec   4096R/3AA5C34371567BD2 2016-03-10 [expires: 2017-03-10]"
      GA.print =<< GA.formatInfo "    A330C91F8EC4BC7AECFA63E03AA5C34371567BD2"
      GA.print =<< GA.formatInfo "    uid                          Hubot"
      GA.print =<< GA.formatInfo ""
      GA.print =<< GA.formatInfo "If you don't want to configure signature, just hit Enter button."
      -- TODO: We could parse IDs out of the gpg output.
      -- Then could ask for the index into list of keys instead?
      key <- GA.promptOneTime "Please pass a key that has to sign objects of the current repository"
      if null key
        then GA.print =<< GA.formatInfo "The signature is not configured as the empty key is provided."
        else do
          GA.setConfigVerbose GA.LocalConfig "user.signingkey" key
          GA.setConfigVerbose GA.LocalConfig "gpg.program" pathToGPG
          GA.setConfigVerbose GA.LocalConfig "commit.gpgsign" "true"
          GA.setConfigVerbose GA.LocalConfig "tag.forceSignAnnotated" "true"
          GA.setConfigVerbose GA.LocalConfig "tag.gpgSign" "true"

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
