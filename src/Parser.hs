{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators          #-}

module Parser where

import           Data.List
import qualified Data.Text                           as T
import qualified Data.Text.IO                        as TIO
import           Text.Parsec                         (alphaNum)
import           Text.ParserCombinators.Parsec
import           Text.ParserCombinators.Parsec.Token


--- Not sure about the types, more considering is required
data Header a where
  NameH :: (a ~ String ) => a -> Header a
  DateH :: (a ~ String) => (a,a,a) -> Header a
  MoodH :: (a ~ String) => [(a,a)] -> Header a -- when writting the show instance, the strings should me mconcated with a newline charecter
  SleepH :: (a ~ String) => (a,a) -> Header a-- when writting the show instance, the strings should me mconcated with a newline charecter
  ProductivityH :: (a ~ String) => (a,a) -> Header a
  MeditationH :: (a ~ String) => [a] -> Header a
  AlcoholH :: (a ~ String) => (a,a) -> Header a
  CigaretteH :: (a ~ String) => (a,a,a) -> Header a
  RatingH :: (a ~ String) => a -> Header a
  AllHeaders :: (a ~ String) => [Header a] -> Header a



type WakeUp = String
type Sleep = String

-- parsing floating numbers as strings
stringFloat :: GenParser Char st Char
stringFloat = digit <|> char '.'

instance Show (Header a) where
  show (NameH a)         = show a
  show (DateH a)         = show a
  show (MoodH a)         = show a
  show (SleepH a)        = show a
  show (ProductivityH a) = show a
  show (MeditationH a)   = show a
  show (AlcoholH a)      = show a
  show (CigaretteH a)    = show a
  show (RatingH a)       = show a
  show (AllHeaders a)    = show a


-- parses '\n' charecters
eol1 :: GenParser Char st String
eol1 = many (char '\n')

-- computes the times of
time :: GenParser Char st String
time = many1 digit <> many1 (char ':') <> many1 digit


header :: String -> GenParser Char st String
header h = string $ mconcat ["[", h, "]"]

-- parses the name section
name :: GenParser Char st String
name = string "Name :"


-- parses the name section
parseName :: forall a st. (a ~ String) => GenParser Char st (Header a)
parseName = do
  name
  spaces
  userName <- many1 alphaNum
  spaces
  userLName <- many1 alphaNum
  eol1
  return $ NameH (userName ++ " " ++  userLName)


-- parses the date section
date :: GenParser Char st String
date = string "Date :" <|> string "Date:"


dateSep :: GenParser Char st Char
dateSep = char '-' <|> char '/' <|> char '_' <|> char '\\'


-- parses the date section
-- parseDate :: forall a st. (a ~ String ) => GenParser Char st (Header a)
-- parseDate = do
--   date
--   spaces
--   userDate <- many1 (alphaNum <|> dateSep)
--   eol1
--   return $ Date userDate

parseDate :: forall a st. (a ~ String) => GenParser Char st (Header a)
parseDate = do
  date
  spaces
  year <- many1 alphaNum
  dateSep
  month <- many1 alphaNum
  dateSep
  day <- many1 alphaNum
  return $ DateH (year,month,day)

-- parses the mood
-- refactor with a list function

mood :: GenParser Char st String
mood = header "Mood"


-- Parses all the mood data constructors as stirngs
parseMood' :: GenParser Char st String
parseMood' = string "Neutral"
         <|> string "Angry"
         <|> string "Sad"
         <|> string "Excited"
         <|> string "Happy"
         <|> string "Focused"
         <|> string "Bored"

-- parses one mood
parseMood :: GenParser Char st (String, String)
parseMood = do
  userMood <- parseMood'
  spaces
  char ':'
  spaces
  moodIntensity <- parseIntensity
  eol1
  return (userMood, moodIntensity)


-- This function type checks but hasn't been tested yet but it's supoosed to parse all the moods
parseMoods :: forall a st. (a ~ String) => GenParser Char st (Header a)
parseMoods = do
  mood
  eol1
  l <- many1 parseMood <* eol1
  return $ MoodH l

-- TODO sort on moods before returning

-- parses all the possible Intensities
parseIntensity :: GenParser Char st String
parseIntensity = string "Low"
             <|> string "Medium"
             <|> string "High"
             <|> string "Extreme"

-- parses the sleep header
sleep :: GenParser Char st String
sleep = header "Sleep"


-- parses the sleep header and it's data
parseSleep :: forall a st. (a ~ String) => GenParser Char st (Header a)
parseSleep = do
  sleep
  eol1
  (string "Wake up :" <* spaces) <|> (string "wake up :" <* spaces)
  wakeUpTime <- time
  eol1
  (string "Sleep :" <* spaces) <|> (string "sleep :" <* spaces)
  sleepTime <- time
  eol1
  return $ SleepH (wakeUpTime, sleepTime)


-- alcohol header
alcohol :: GenParser Char st String
alcohol = header "Alcohol"


-- parses the alcohol header and the data in it
parseAlcohol :: forall a st. (a ~ String) => GenParser Char st (Header a)
parseAlcohol = do
  alcohol
  eol1
  drink <- (many1 alphaNum <* spaces) <* string ":"
  shots <- spaces *> many1 digit
  eol1
  return $ AlcoholH (drink,shots)


-- parses the cigarette header
cigarette :: GenParser Char st String
cigarette = header "Cigarette"


-- parses the cigarette header and the data in it
parseCigarette :: forall a st. (a ~ String) => GenParser Char st (Header a)
parseCigarette = do
  cigarette
  eol1
  (string "Number :" <* spaces) <|> (string "number :" <* spaces)
  number <- many1 digit
  eol1
  (string "Nicotine :" <* spaces) <|> (string "nicotine :" <* spaces)
  nicotine <- many1 stringFloat
  eol1
  (string "Tar :" <* spaces) <|> (string "tar :" <* spaces)
  tar <- many1 stringFloat
  eol1
  return $ CigaretteH (number,nicotine,tar)


-- parses the meditation header
meditation :: GenParser Char st String
meditation = header "Meditation"

-- parses the meditatin header and the data in it
parseMeditations :: forall a st. (a ~ String) => GenParser Char st (Header a)
parseMeditations = do
  meditation
  eol1
  meditations <- many1 (many1 (digit <|> char ':') <* eol1)
  eol1
  return $ MeditationH meditations


productivity :: GenParser Char st String
productivity = header "Productivity"

-- parses the productivity header and the information in it
parseProductivity :: forall a st. (a ~ String) => GenParser Char st (Header a)
parseProductivity = do
  productivity
  eol1
  done <- many1 digit <* char '/'
  shouldHave <- many1 digit
  eol1
  return $ ProductivityH (done,shouldHave)


-- parses the rating header
rating :: GenParser Char st String
rating = header "Rating"


-- convRating :: GenParser Char st String -> Header a
-- convRating

-- parses the different rating a user might give
parseRating' :: GenParser Char st String
parseRating' = string "Great"
           <|> string "Good"
           <|> string "Normal"
           <|> string "Bad"
           <|> string "Awful"

-- parses the whole Rating header (section)
parseRating :: forall a st. (a ~ String) => GenParser Char st (Header a)
parseRating = do
  rating
  eol1
  prsd <- parseRating'
  return $ RatingH prsd


--parses the Entry written by the user
parseEntry :: forall a st. (a ~ String) => GenParser Char st (Header a)
parseEntry = do
  n <- parseName
  d <- parseDate
  m <- parseMoods
  s <- parseSleep
  al <- parseAlcohol
  me <- parseMeditations
  c <- parseCigarette
  p <- parseProductivity
  r <- parseRating
  return $ AllHeaders [n,d,m,s,al,me,c,p,r]


-- parses the info section of user's config file
parseInfo :: GenParser Char st [String]
parseInfo = do
  string "Info :"
  eol1
  spaces
  string "name =" <* spaces
  name <- many1 alphaNum
  spaces
  lName <- many1 alphaNum
  eol1 >> spaces
  string "email =" <* spaces
  email <- many1 (alphaNum <|> char '.' <|> char '@')
  eol1
  return [name,lName,email]

-- parses the daemon section of user's config file
parseDaemon :: GenParser Char st String
parseDaemon = do
  string "Daemon :"
  eol1
  spaces
  string "run_daemon" >> spaces >> char '=' >> spaces
  value <- string "True" <|> string "False"
  return value


parseOptionalHeaders :: GenParser Char st [String]
parseOptionalHeaders = do
  string "optional_headers ="
  spaces
  sepBy (string "Meditation" <|> string "Alcohol" <|> string "Cigarette") (string " - " <|> string "-") :: GenParser Char st [String]


-- parses the template section of user's config file
parseTemplate :: GenParser Char st (String,[String])
parseTemplate = do
  string "Template :"
  eol1
  spaces
  generateTemplate <- (string "generate_template =" >> spaces) *> (string "True" <|> string "False")
  eol1
  spaces
  optionalHeaders <- parseOptionalHeaders
  return (generateTemplate,optionalHeaders)


-- parses the report section of user's config file
parseReport :: GenParser Char st [String]
parseReport = do
  string "Report :"
  eol1
  spaces
  string "email_report ="
  spaces
  emailReport <- string "True" <|> string "False"
  eol1
  spaces
  string "email_report_frequency ="
  spaces
  emailReportFrequencyN <- many1 digit
  spaces
  emailReportFrequencyD <- string "Month" <|> string "Week" <|> string "Year"
  return [emailReport,emailReportFrequencyN,emailReportFrequencyD]
