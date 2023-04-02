module Lib where

import qualified Elegit.Cli.Action.AcquireRepository as AcquireRepository
import qualified Elegit.Cli.Action.InitRepository    as InitRepository
import qualified Elegit.Cli.Action.ShowWork          as ShowWork
import           Elegit.Cli.Command                  (ElegitCommand (..))
import qualified Elegit.Cli.Parser                   as P
import           Elegit.Git.Exec                     (GitExecT (runGitExecT))
import           Elegit.Git.Runner.Real              (executeGit)
import           Options.Applicative                 (customExecParser)
import           Universum

runCli :: (MonadMask m, MonadIO m) => m ()
runCli = do
  cmd <- liftIO $ customExecParser P.cliPrefs P.cli
  let
    errHandler se = case se of
                     Exc (Bug e cs) -> do
                       putTextLn $ "Caught exception: " <> show e
                       putStrLn $ prettyCallStack cs
                     _ -> putTextLn $ "Caught exception: " <> show se

  flip catch errHandler $
    runGitExecT $
    executeGit $
      case cmd of
         ShowWorkCommand          -> ShowWork.cmd
         AcquireRepositoryCommand -> AcquireRepository.cmd
         InitRepositoryCommand    -> InitRepository.cmd
