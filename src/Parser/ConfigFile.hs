module Parser.ConfigFile where

import           Control.Monad.Trans.Reader    ()

import           Text.Parsec                   ( alphaNum )
import           Text.Parsec.Char              ( newline )
import           Text.ParserCombinators.Parsec ( GenParser, alphaNum, char,
                                                 choice, digit, many, many1,
                                                 sepBy, spaces, string, (<|>) , oneOf, parse)

import Config



tOf :: GenParser Char st Bool
tOf = read <$> (choice $ map string ["True", "False"])

parseInfo :: GenParser Char st InfoConf
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
parseDaemon :: GenParser Char st DaemonConfig
parseDaemon = do
  string "Daemon :"
  many newline
  spaces
  string "run_daemon" >> spaces >> char '=' >> spaces
  value <- string "True" <|> string "False"
  return $ DConf (read value :: Bool)


parseOptionalHeaders :: GenParser Char st OptHeader
parseOptionalHeaders = do
  string "optional_headers :" >> newline >> spaces
  med <- string "meditation = " >> tOf <* (newline >> spaces)
  alc <- string "alcohol = " >> tOf <* (newline >> spaces)
  cig <- string "cigarette = " >> tOf <* (newline >> spaces)
  return $ OptH med alc cig


parseGenTemplate :: GenParser Char st GenTemplate
parseGenTemplate = do
  string "generate_template = "
  val <- tOf
  return $ GenTemp val


-- parses the template section of user's config file
parseTemplate :: GenParser Char st TemplateConf
parseTemplate = do
  string "Template :"
  newline >> spaces
  genTemp <- parseGenTemplate
  newline >> spaces
  optionalHeaders <- parseOptionalHeaders
  return $ TempConf genTemp optionalHeaders

-- parses the report section of user's config file
parseReport :: GenParser Char st ReportConf
parseReport = do
  string "Report :" >> newline >>  spaces
  string "email_report =" >> spaces
  emailRep <- tOf
  many newline >>  spaces
  string "email_report_frequency =" >> spaces
  emailReportFreq <- many1 digit <* spaces
  return $ RepConf emailRep (read emailReportFreq)


-- parses all of the config file and returns a config type to pass to the Reader Monad
parseConfig :: GenParser Char st Config
parseConfig = do
  i <- parseInfo
  d <- parseDaemon
  t <- parseTemplate
  r <- parseReport
  return $ Config i d t r
