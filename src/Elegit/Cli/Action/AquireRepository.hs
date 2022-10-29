module Elegit.Cli.Action.AquireRepository where

import           Control.Monad.Free.Class
import qualified Elegit.Git.Action        as GA
import           Universum

-- | Exectuion description of the AquireRepository action
cmd :: (MonadFree GA.GitF m) => Text -> m ()
cmd repo = do
    GA.cloneRepository repo

    GA.defaultUsername
        >>= GA.promptDefault "Your username"
        >>= GA.updateConfig "user.name"
    GA.defaultEmail
        >>= GA.promptDefault "Your email"
        >>= GA.updateConfig "user.email"
    GA.defaultEditor
        >>= GA.promptDefault "Default code editor"
        >>= GA.updateConfig "core.editor"

    GA.updateConfig "core.commentChar" "|"
    GA.updateConfig "apply.whitespace" "fix"
    GA.updateConfig "fetch.prune" "true"
    GA.updateConfig "fetch.pruneTags" "false"
    GA.updateConfig "core.autoclrf" "input"
    GA.updateConfig "pull.rebase" "true"
    GA.updateConfig "rebase.autostash" "false"
    GA.updateConfig "credential.helper" "osxkeychain"
