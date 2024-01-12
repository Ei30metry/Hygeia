{-# LANGUAGE OverloadedStrings #-}

module CLI where

import           Computation.Monad
import           Computation.Types

import           Config

import           Control.Lens
import           Control.Monad

import qualified Data.ByteString             as B
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


-- probably have to change the location of this
-- defaultConfig :: Config
-- instead of subcommands for parseGenerate and parseSummary, use options

parseGenerate, parseDaemon, parseSummary :: Parser Action
parseGenerate = undefined
parseDaemon   = undefined
parseSummary  = undefined
-- use some to get mul
parseConfig   = undefined


dateOption :: Parser Interval
dateOption  = Date <$> option dateParser (long "date" <> short 'D' <> metavar "YY-MM-DD")
  where
    parseDate = P.many (P.digit <|> P.char '-')
    dateParser = eitherReader $ \s -> case P.runParser parseDate () "" s of
                                           Right x -> join $ Right (readEither x)
                                           Left e  -> Left (show e)

-- value All
dayOption, weekOption, monthOption, yearOption :: Parser Interval
dayOption   = Day <$> option auto (long "day" <> short 'd' <> metavar "n")
weekOption  = Week <$> option auto (long "week" <> short 's' <> metavar "n")
monthOption = Month <$> option auto (long "month" <> short 'm' <> metavar "n")
yearOption  = Year <$> option auto (long "year" <> short 'y' <> metavar "n")


parseCommand = subparser $ genCommand <> summCommand <> confCommand
  where genCommand = command "generate" (info parseGenerate (progDesc "Generate an entry template"))
        summCommand = command "summary" (info parseSummary (progDesc "Show summary of the entries"))
        confCommand = command "config" (info parseSummary (progDesc "Configuration"))

cli = undefined
