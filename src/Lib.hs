module Lib where

import qualified Elegit.Cli.Action.AcquireRepository as AcquireRepository
import qualified Elegit.Cli.Action.ShowWork          as ShowWork
import           Elegit.Cli.Command                  (ElegitCommand (..))
import qualified Elegit.Cli.Parser                   as P
import           Elegit.Git.Runner.Real              (executeGit)
import           Options.Applicative                 (customExecParser)
import           Universum

runCli :: (MonadIO m, MonadCatch m) => m ()
runCli = do
  cmd <- liftIO $ customExecParser P.cliPrefs P.cli

  flip catch (\e -> putTextLn $ "Caught exception: " <> show (e :: SomeException) ) $
    executeGit $
      case cmd of
         ShowWorkCommand          -> ShowWork.cmd
         AcquireRepositoryCommand -> AcquireRepository.cmd
