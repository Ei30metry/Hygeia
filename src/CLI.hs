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

- daemon:
   - start
   - restart
   - shutdown


- generate:
   - default: Template for today
   - options:
      date YY-MM-DD: Set a manual date for entry
      day n:
      month n:
      year n:

- summary: moods cigarette and all the other headers, the user should be able to specify the things he wants specificatlly
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


-- verbosity flag that prints the exact entries.
-- probably have to change the location of this


-- defaultConfig :: Config
-- instead of subcommands for parseGenerate and parseSummary, use options

{-
default value for Interval differs for every action.

In Generate, it's Day "today"
In Summary, it's All
we would need a convinient way to convey this information

-}
parseGenerate, parseDaemon, parseSummary, parseConfig :: Parser Action
parseGenerate = Generete <$> parseInterval

-- TODO Should print available operations to the user
-- TODO Use error handling facilities provided by Options.Applicative instead of throwError
parseDaemon = Daemon <$> argument (eitherReader helper) (metavar "METAVAR")
  where helper "start"    = pure Start
        helper "stop"     = pure Shutdown
        helper "restart"  = pure Restart
        helper "shutdown" = pure Shutdown
        helper x          = throwError $ show x ++ " is not a daemon operation."

-- TODO Should print available entry fields to the user
parseSummary = Summary <$> parseEntryField <*> parseInterval
  where
    parseEntryField = some (argument (eitherReader helper) (metavar "ENTRYFIELD"))
    helper "mood"         = Right MoodField
    helper "meditation"   = Right MeditationField
    helper "cigarette"    = Right CigaretteField
    helper "drink"        = Right DrinkField
    helper "sleep"        = Right SleepField
    helper "productivity" = Right ProductivityField
    helper x              = Left $ show x ++ " is not an entry field."

-- NOTE we shold do a check to see if the value passed is actually acceptable
parseConfig = M.Config <$> subparser (catCommand <> editCommand <> setCommand)
  where
    editCommand = command "edit" (info (pure Edit) (progDesc "open config file in $EDITOR"))
    catCommand = command "cat" (info catC (progDesc "cat config field"))
    setCommand = command "set" (info setC (progDesc "change the value of a config file entry"))

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
      | x `elem` ["true", "false", "on"
                 ,"off", "true", "false"] = pure . BVal . readBoolish $ map toLower x

      | otherwise = pure (SVal x)

    helper' "template"        = Right TemplateField
    helper' "entry-directory" = Right EntryDirectoryField
    helper' "daemon"          = Right DaemonField
    helper' "info"            = Right UserInfoField
    helper' x = optHeader =<< case splitOn ":" x of
                   ["optional-headers", x] -> Right x
                   _                       -> Left "Unknown config field"
      where
        optHeader xs = OptionalHeaderField <$> case xs of
            "meditation" -> Right OMeditation
            "alcohol"    -> Right ODrink
            "cigarette"  -> Right OCigarette
            unknown      -> Left ("Uknown Optional Header, " ++ unknown)


dateOption :: Parser Interval
dateOption  = Date <$> option dateParser (long "date" <> short 'D' <> metavar "YY-MM-DD")
  where
    parseDate = P.many (P.digit <|> P.char '-')
    dateParser = eitherReader $ \s -> case P.runParser parseDate () "" s of
                                           Right x -> join $ Right (readEither x)
                                           Left e  -> Left (show e)

-- NOTE default value All
dayOption, weekOption, monthOption, yearOption :: Parser Interval
dayOption   = Day <$> option auto (long "day" <> short 'd' <> metavar "n")
weekOption  = Week <$> option auto (long "week" <> short 's' <> metavar "n")
monthOption = Month <$> option auto (long "month" <> short 'm' <> metavar "n")
yearOption  = Year <$> option auto (long "year" <> short 'y' <> metavar "n")


parseInterval = dayOption <|> weekOption <|> monthOption <|> yearOption


parseCommand = subparser $ genCommand <> summCommand <> confCommand <> daemonCommand
  where genCommand    = command "generate" (info parseGenerate (progDesc "Generate an entry template"))
        summCommand   = command "summary" (info parseSummary (progDesc "Show summary of the entries"))
        confCommand   = command "config" (info parseConfig (progDesc "Configuration"))
        daemonCommand = command "daemon" (info parseDaemon (progDesc "Daemon"))


cli :: IO Action
cli = execParser (info (parseCommand <**> helper)
                          (fullDesc <> progDesc "blah"
                                    <> header "Hygeia"))
