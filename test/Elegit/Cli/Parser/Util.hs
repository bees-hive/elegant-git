module Elegit.Cli.Parser.Util where


import           Elegit.Cli.Command  (ElegitCommand (..))
import           Elegit.Cli.Parser   as P
import           Options.Applicative
import           Test.Hspec          (Expectation, expectationFailure, shouldBe)
import           Universum


parseMaybe :: Text -> [Text] -> Maybe ElegitCommand
parseMaybe cmd args =
    getParseResult $ parseResult cmd args

parseResult :: Text -> [Text] -> ParserResult ElegitCommand
parseResult cmd args =
    execParserPure P.cliPrefs P.cli (toString cmd: map toString args)

shouldRender :: (Show a) => ParserResult a -> Text -> Expectation
shouldRender parserResult expectedOutput =
    case parserResult of
        Failure pFailure ->
            toText (fst $ renderFailure pFailure "git elegant") `shouldBe` expectedOutput
        _ -> expectationFailure $
            "Parser did not render anything. Parsing result: " <> show parserResult
