module Lib where

import qualified Elegit.Cli.Action.ShowWork as ShowWork
import           Elegit.Cli.Command         (ElegitCommand (..))
import qualified Elegit.Cli.Parser          as P
import           Elegit.Git.Runner.Real     (executeGit)
import           Options.Applicative        (customExecParser, prefs,
                                             showHelpOnEmpty, showHelpOnError)
import           Universum

runCli :: (MonadIO m, MonadCatch m) => m ()
runCli = do
  let
    cliPrefs = prefs $ showHelpOnError <> showHelpOnEmpty

  cmd <- liftIO $ customExecParser cliPrefs P.cli

  flip catch (\e -> putTextLn $ "Caugth exception: " <> show (e :: SomeException) ) $
    executeGit $
      case cmd of
         ShowWorkCommand -> ShowWork.cmd
