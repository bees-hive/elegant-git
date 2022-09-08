module Elegit.Cli.Parser where

import           Elegit.Cli.Command  (ElegitCommand (ShowWorkCommand))
import           Options.Applicative
import           Universum


type Command a = Mod CommandFields a


showWorkCommand :: Command ElegitCommand
showWorkCommand =
    command "show-work" $
        flip info (progDesc "Prints HEAD state.") $ pure ShowWorkCommand


dayToDayContributionsCommand :: Command ElegitCommand
dayToDayContributionsCommand =
    commandGroup " make day-to-day contributions"
    <> showWorkCommand


cli :: ParserInfo ElegitCommand
cli = flip info mempty $
    hsubparser dayToDayContributionsCommand <**> helper

