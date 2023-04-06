module Elegit.Cli.Parser where

import qualified Elegit.Cli.Action.AcquireRepository as AcquireRepository
import qualified Elegit.Cli.Action.InitRepository    as InitRepository
import qualified Elegit.Cli.Action.ShowWork          as ShowWork
import           Elegit.Cli.Command
import           Options.Applicative
import           Universum


type Command a = Mod CommandFields a


dayToDayContributionsCommand :: Command ElegitCommand
dayToDayContributionsCommand =
  commandGroup "make day-to-day contributions"
  <> ShowWork.cli


enableElegantGitServices :: Command ElegitCommand
enableElegantGitServices =
  commandGroup "enable Elegant Git services"
  <> AcquireRepository.cli
  <> InitRepository.cli


cli :: ParserInfo ElegitCommand
cli = flip info mempty $
  (hsubparser enableElegantGitServices
   <|> hsubparser dayToDayContributionsCommand)
  <**> helper


cliPrefs :: ParserPrefs
cliPrefs = prefs $
  mconcat [ showHelpOnError
          , showHelpOnEmpty
          , noBacktrack
          ]

