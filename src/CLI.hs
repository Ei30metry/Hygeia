{-# LANGUAGE OverloadedStrings #-}

module CLI where

import           Computation.Monad
import qualified Computation.Monad as M

import           Config

import           Control.Monad
import           Control.Monad.Except

import qualified Data.ByteString             as B
import           Data.Char                   ( toLower )
import           Data.List.Split             ( splitOn )
import           Data.Time                   ( Day )

import           Options.Applicative
import           Options.Applicative.Builder

import qualified Text.Parsec                 as P
import           Text.Read                   ( readEither )

{-

Command line interface

The user should be able to create a config by using the cli interface, the generated config should be written to XDG_CONFIG

commands:

hygeia generate
hygeia summary
hygeia config


Subcommands:

- lookup
   - EntryField

- generate:
   - default: Template for today
   - options:
      date YY-MM-DD: Set a manual date for entry
      day n:
      month n:
      year n:

- summary: moods cigarette and all the other headers, the user should be able to specify the things he wants specifically
   - nothing
   - default: all the entries
   - date YY-MM-DD
   - day n
   - month n
   - week n
   - year n

- config:
  - edit: use the $EDITOR
  - cat:
     - nothing
     - name
     - email
     - daemon
     - optHeaders
     - entryDir
     - template
  - set:
     - name
     - email
     - daemon
     - optHeaders
     - entryDir
     - template

-}
parseEntryField :: Parser [EntryField]
parseEntryField = some (argument (eitherReader helper) (metavar "ENTRYFIELD"))
  where
    helper "Mood"         = Right MoodField
    helper "Meditation"   = Right MeditationField
    helper "Cigarette"    = Right CigaretteField
    helper "Drink"        = Right DrinkField
    helper "Sleep"        = Right SleepField
    helper "Productivity" = Right ProductivityField
    helper x              = Left $ show x ++ " is not an entry field." -- NOTE should print all the available values


parseGenerate, parseLookup, parseSummary, parseConfig :: Parser Action
parseGenerate = Generate <$> parseInterval Today

-- NOTE Should print available entry fields to the user
parseSummary = Summary <$> parseEntryField <*> parseInterval All


parseLookup = Lookup <$> parseEntryField <*> parseInterval All

-- NOTE we shold do a check to see if the value passed is actually acceptable
parseConfig = M.Config <$> subparser (catCommand <> editCommand <> setCommand)
  where
    editCommand =
      command "edit" (info (pure Edit) (progDesc "open config file in $EDITOR"))

    catCommand = command "cat" (info catC (progDesc "cat config field"))

    setCommand =
      command "set" (info setC (progDesc "change the value of a config file entry"))

    catC = Cat <$> configField
    configField = argument (eitherReader helper') (metavar "configfield")
    setC = Set <$> configField <*> parseFieldValue
    parseFieldValue = argument (eitherReader vReader) (metavar "Value")
    readBoolish x = case x of
      "true"  -> True
      "on"    -> True
      "false" -> False
      "off"   -> False
    vReader x
      | x `elem` ["true", "false", "on", "off", "true", "false"] =
        pure . BVal . readBoolish $ map toLower x
      | otherwise = pure (SVal x)

    helper' "template" = Right TemplateField
    helper' "entry-directory" = Right EntryDirectoryField
    helper' "daemon" = Right DaemonField
    helper' "info" = Right UserInfoField
    helper' x =
      optHeader =<<
      case splitOn ":" x of
        ["optional-headers", x] -> Right x
        _ -> Left "Unknown config field"
      where
        optHeader xs =
          OptionalHeaderField <$>
          case xs of
            "meditation" -> Right OMeditation
            "alcohol" -> Right ODrink
            "cigarette" -> Right OCigarette
            unknown -> Left ("Uknown Optional Header, " ++ unknown)


dateOption, dayOption, weekOption, monthOption, yearOption :: Parser Interval
dayOption   = Days   <$> option (abs <$> auto) (long "day" <> short 'd' <> metavar "N")
weekOption  = Weeks  <$> option (abs <$> auto) (long "week" <> short 'w' <> metavar "N")
monthOption = Months <$> option (abs <$> auto) (long "month" <> short 'm' <> metavar "N")
yearOption  = Years  <$> option (abs <$> auto) (long "year" <> short 'y' <> metavar "N")
dateOption =
  Date <$> option dateParser (long "date" <> short 'D' <> metavar "YYYY-MM-DD")
  where
    parseDate = P.many (P.digit <|> P.char '-')
    dateParser =
      eitherReader $ \s ->
        case P.runParser parseDate () "" s of
          Right x -> join $ Right (readEither x)
          Left _ -> Left "Wrong date format"


parseInterval :: DefaultInterval -> Parser Interval
parseInterval dval = dayOption <|> dateOption
                  <|> weekOption <|> monthOption
                  <|> yearOption <|> pure (DefInterval dval)


parseCommand :: Parser Action
parseCommand = subparser $ genCommand <> summCommand <> confCommand <> lookupCommand
  where genCommand    = command "generate" (info parseGenerate (progDesc "Generate an entry template"))
        summCommand   = command "summary" (info parseSummary (progDesc "Show summary of the entries"))
        lookupCommand = command "lookup" (info parseLookup (progDesc "Lookup entries"))
        confCommand   = command "config" (info parseConfig (progDesc "Configuration"))


cli :: IO Action
cli = execParser (info (parseCommand <**> helper)
                          (fullDesc <> progDesc "A simple mood and habit tracker"
                                    <> header "Hygeia"))
