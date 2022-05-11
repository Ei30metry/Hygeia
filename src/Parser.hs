{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}

module Parser where

import qualified Data.Text                           as T
import qualified Data.Text.IO                        as TIO
import           Text.ParserCombinators.Parsec
import           Text.ParserCombinators.Parsec.Token


--- Not sure about the types, more considering is required
data Header a where
  Name :: (a ~ String ) => a -> Header a
  Date :: (a ~ String) => a -> Header a
  MoodH :: (a ~ String) => [a] -> Header a -- when writting the show instance, the strings should me mconcated with a newline charecter
  Sleep :: (a ~ String) => [a] -> Header a -- when writting the show instance, the strings should me mconcated with a newline charecter
  Productivity :: (a ~ String) => a -> Header a
  Meditation :: (a ~ String) => [a] -> Header a
  Alcohol :: (a ~ String) => a -> Header a
  Cigarette :: (a ~ String) => a -> Header a
  Rating :: (a ~ String) => a -> Header a

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
name = string "Name :" <|> string "Name: "

parseName :: GenParser Char st String
parseName = do
  name
  spaces
  userName <- many1 alphaNum
  spaces
  userLName <- many1 alphaNum
  return $ userName ++ " " ++  userLName

-- parses the date section
date :: GenParser Char st String
date = string "Date :" <|> string "date :" <|> string "Date:" <|> string "date:"

dateSep :: GenParser Char st Char
dateSep = char '-' <|> char '/' <|> char '_' <|> char '\\'

parseDate :: GenParser Char st String
parseDate = do
  date
  spaces
  userDate <- many1 (alphaNum <|> dateSep)
  return userDate

-- parses the mood
-- refactor with a list function

mood :: GenParser Char st String
mood = header "Mood" <|> header "mood"


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
  return (userMood, moodIntensity)

-- This function type checks but hasn't been tested yet but it's supoosed to parse all the moods
parseMoods :: GenParser Char st [(String,String)]
parseMoods = do
  mood
  eol1
  many1 parseMood <* eol1


-- parses all the possible Intensities
parseIntensity :: GenParser Char st String
parseIntensity = string "Low"
             <|> string "Normal"
             <|> string "High"
             <|> string "Extreme"

-- parses the sleep header
sleep :: GenParser Char st String
sleep = header "Sleep" <|> header "sleep"

sleepTest :: GenParser Char st String
sleepTest = string "Sleep" <|> header "sleep"

-- parses the sleep section
parseSleep :: GenParser Char st (String,String)
parseSleep = do
  sleep
  eol1
  (string "Wake up :" <* spaces) <|> (string "wake up :" <* spaces)
  wakeUpTime <- time
  eol1
  (string "Sleep :" <* spaces) <|> (string "sleep :" <* spaces)
  sleepTime <- time
  return (wakeUpTime, sleepTime)


--parses the productivity header
productivity :: GenParser Char st String
productivity = header "Productivity" <|> header "productivity"

-- alcohol header
alcohol :: GenParser Char st String
alcohol = header "Alcohol" <|> header "alcohol"


-- parseAlcohol :: GenParser Char st [String]
-- parseAlcohol = do
--   header
--   eol1
--   many1 (many1 alphaNum )



-- parses productivity into a tuple which has the done tasks as its fst and assigned tasks as its snd
parseProductivity :: GenParser Char st (String,String)
parseProductivity = do
  productivity
  eol1
  done <- many1 digit <* char '/'
  shouldHave <- many1 digit
  return (done,shouldHave)

-- parses the rating header
rating :: GenParser Char st String
rating = header "Rating" <|> header "rating"

-- parses the different rating a user might give
parseRating' :: GenParser Char st String
parseRating' = string "Great"
           <|> string "Good"
           <|> string "Neutral"
           <|> string "Bad"
           <|> string "Awful"

-- parses the whole Rating header (section)
parseRating :: GenParser Char st String
parseRating = do
  rating
  parseRating'


-- parses the Entry written by the user
parseEntry :: IO T.Text -> [Header String]
parseEntry = undefined
