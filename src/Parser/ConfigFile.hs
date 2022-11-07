module Parser.ConfigFile where

import           Text.Parsec                         ( alphaNum )
import           Text.ParserCombinators.Parsec       ( GenParser, alphaNum,
                                                       char, choice, digit,
                                                       many, many1, sepBy,
                                                       spaces, string, (<|>) )
import Text.Parsec.Char (newline)


type ConfCategoryName = String
type ConfValue = String
type ConfOptions = String



configParser :: ConfCategoryName -> [(ConfValue, [ConfOptions])] -> GenParser Char st [String]
configParser = undefined


parseInfo :: GenParser Char st [String]
parseInfo = do
  string "Info :"
  many newline
  spaces
  string "name =" <* spaces
  name <- many1 alphaNum
  spaces
  lName <- many1 alphaNum
  many newline >> spaces
  string "email =" <* spaces
  email <- many1 (alphaNum <|> char '.' <|> char '@')
  many newline
  return [name,lName,email]

-- parses the daemon section of user's config file
parseDaemon :: GenParser Char st String
parseDaemon = do
  string "Daemon :"
  many newline
  spaces
  string "run_daemon" >> spaces >> char '=' >> spaces
  value <- string "True" <|> string "False"
  return value


parseOptionalHeaders :: GenParser Char st [String]
parseOptionalHeaders = do
  string "optional_headers ="
  spaces
  sepBy (choice $ map string ["Meditation", "Alcohol", "Cigarette"]) (string " - " <|> string "-")


-- parses the template section of user's config file
parseTemplate :: GenParser Char st (String,[String])
parseTemplate = do
  string "Template :"
  many newline
  spaces
  generateTemplate <- (string "generate_template =" >> spaces) *> (string "True" <|> string "False")
  many newline
  spaces
  optionalHeaders <- parseOptionalHeaders
  return (generateTemplate,optionalHeaders)


-- parses the report section of user's config file
parseReport :: GenParser Char st [String]
parseReport = do
  string "Report :"
  many newline
  spaces
  string "email_report ="
  spaces
  emailReport <- string "True" <|> string "False"
  many newline
  spaces
  string "email_report_frequency ="
  spaces
  emailReportFrequencyN <- many1 digit
  spaces
  emailReportFrequencyD <- choice $ map string ["Month", "Week", "Year"]
  return [emailReport,emailReportFrequencyN,emailReportFrequencyD]
