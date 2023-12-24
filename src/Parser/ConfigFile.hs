module Parser.ConfigFile where

import           Config

import           Text.Parsec                   ( alphaNum )
import           Text.Parsec.Char              ( newline )
import           Text.Parsec.String            ( Parser )                     -- TODO Migrate to Text.Parsec.ByteString
import           Text.ParserCombinators.Parsec ( alphaNum, char, choice, digit,
                                                 many, many1, oneOf, parse,
                                                 sepBy, spaces, string, (<|>) )
import           Text.Read                     ( readEither )

-- TODO: use readEither and hoist it into Parser
tOf :: Parser Bool
tOf = read <$> choice (map string ["True", "False"])

parseInfo :: Parser InfoConf
parseInfo = do
  string "Info :"
  many newline >> spaces
  string "name =" <* spaces
  name <- many1 alphaNum
  spaces
  lName <- many1 alphaNum
  many newline >> spaces
  string "email =" <* spaces
  email <- many1 (alphaNum <|> char '.' <|> char '@')
  many newline
  return $ Info (name ++ " " ++ lName) email

-- parses the daemon section of user's config file
parseDaemon :: Parser DaemonConfig
parseDaemon = do
  string "Daemon :"
  many newline
  spaces
  string "run_daemon" >> spaces >> char '=' >> spaces
  value <- string "True" <|> string "False"
  return $ DConf (read @Bool value)


parseOptionalHeaders :: Parser OptHeader
parseOptionalHeaders = do
  string "optional_headers :" >> newline >> spaces
  med <- string "meditation = " >> tOf <* (newline >> spaces)
  alc <- string "alcohol = " >> tOf <* (newline >> spaces)
  cig <- string "cigarette = " >> tOf <* (newline >> spaces)
  return $ OptH med alc cig


parseGenTemplate :: Parser GenTemplate
parseGenTemplate = string "generate_template = " >>  GenTemp <$> tOf


-- parses the template section of user's config file
parseTemplate :: Parser TemplateConf
parseTemplate = do
  string "Template :"
  newline >> spaces
  genTemp <- parseGenTemplate
  newline >> spaces
  TempConf genTemp <$> parseOptionalHeaders

-- parses the report section of user's config file
parseReport :: Parser ReportConf
parseReport = do
  string "Report :" >> newline >>  spaces
  string "email_report =" >> spaces
  emailRep <- tOf
  many newline >>  spaces
  string "email_report_frequency =" >> spaces
  emailReportFreq <- many1 digit <* spaces
  return $ RepConf emailRep (read emailReportFreq)


-- parses all of the config file and returns a config type to pass to the Reader Monad
parseConfig :: Parser Config
parseConfig = do
  i <- parseInfo
  d <- parseDaemon
  t <- parseTemplate
  Config i d t <$> parseReport
