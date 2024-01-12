module Parser.Config where

import           Config

import           Control.Monad.Except  ( liftEither )

import           Data.ByteString.Char8 ( ByteString, pack )

import           Parser.Monad

import           Text.Parsec.Char      ( newline )
import           Text.Read             ( readEither )

-- TODO: use readEither and hoist it into Parser
-- tOf :: Parser Bool
-- tOf = liftEither . readEither =<< choice (map string ["True", "False"])

-- parseInfo :: Parser UserInfo
-- parseInfo = do
--   string "Info :"
--   many newline >> spaces
--   string "name =" <* spaces
--   name <- pack <$> many1 alphaNum
--   spaces
--   lName <- pack <$> many1 alphaNum
--   many newline >> spaces
--   string "email =" <* spaces
--   email <- pack <$> many1 (alphaNum <|> char '.' <|> char '@')
--   many newline
--   return $ Info (name <> " " <> lName) email

-- -- parses the daemon section of user's config file
-- parseDaemon :: Parser DaemonConf
-- parseDaemon = do
--   string "Daemon :"
--   many newline
--   spaces
--   string "run_daemon" >> spaces >> char '=' >> spaces
--   value <- liftEither . readEither =<< string "True" <|> string "False"
--   return $ DaemonConf value


-- parseOptionalHeaders :: Parser OptHeader
-- parseOptionalHeaders = do
--   string "optional_headers :" >> newline >> spaces
--   med <- string "meditation = " >> tOf <* (newline >> spaces)
--   alc <- string "alcohol = " >> tOf <* (newline >> spaces)
--   cig <- string "cigarette = " >> tOf <* (newline >> spaces)
--   return $ OptH med alc cig


-- parseGenTemplate :: Parser GenTemplate
-- parseGenTemplate = string "generate_template = " >> GenTemp <$> tOf


-- -- parses the template section of user's config file
-- parseTemplate :: Parser TemplateConf
-- parseTemplate = do
--   string "Template :"
--   newline >> spaces
--   genTemp <- parseGenTemplate
--   newline >> spaces
--   TempConf genTemp <$> parseOptionalHeaders

-- -- parses the report section of user's config file
-- parseReport :: Parser ReportConf
-- parseReport = do
--   string "Report :" >> newline >>  spaces
--   string "email_report =" >> spaces
--   emailRep <- tOf
--   many newline >>  spaces
--   string "email_report_frequency =" >> spaces
--   emailReportFreq <- many1 digit <* spaces
--   emailRRep <- liftEither (readEither emailReportFreq)
--   return $ RepConf emailRep emailRRep


-- -- parses all of the config file and returns a config type to pass to the Reader Monad
-- parseConfig :: Parser Config
-- parseConfig = do
--   i <- parseUserInfo
--   d <- parseDaemon
--   t <- parseTemplate
--   Config i d t <$> parseReport
