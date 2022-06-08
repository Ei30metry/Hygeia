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

import qualified Data.Text                           as T
import qualified Data.Text.IO                        as TIO
import           Text.ParserCombinators.Parsec
import           Text.ParserCombinators.Parsec.Token


--- Not sure about the types, more considering is required
data Header a where
  Name :: (a ~ String ) => a -> Header a
  Date :: (a ~ String) => a -> Header a
  MoodH :: (a ~ String) => [a] -> Header a -- when writting the show instance, the strings should me mconcated with a newline charecter
  Sleep :: (a ~ String) => (a,a) -> Header (a,a)-- when writting the show instance, the strings should me mconcated with a newline charecter
  Productivity :: (a ~ String) => (a,a) -> Header (a,a)
  Meditation :: (a ~ String) => [a] -> Header [a]
  Alcohol :: (a ~ String) => (a,a) -> Header (a,a)
  Cigarette :: (a ~ String) => (a,a,a) -> Header (a,a,a)
  Rating :: (a ~ String) => a -> Header a


type WakeUp = String
type Sleep = String


instance Show (Header a) where
  show (Name a)         = show a
  show (Date a)         = show a
  show (MoodH a)        = show a
  show (Sleep a)        = show a
  show (Productivity a) = show a
  show (Meditation a)   = show a
  show (Alcohol a)      = show a
  show (Cigarette a)    = show a
  show (Rating a)       = show a

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


parseName :: forall a st. (a ~ String) => GenParser Char st (Header a)
parseName = do
  name
  spaces
  userName <- many1 alphaNum
  spaces
  userLName <- many1 alphaNum
  return $ Name (userName ++ " " ++  userLName)

-- parses the date section
date :: GenParser Char st String
date = string "Date :" <|> string "date :" <|> string "Date:" <|> string "date:"

dateSep :: GenParser Char st Char
dateSep = char '-' <|> char '/' <|> char '_' <|> char '\\'


parseDate :: forall a st. (a ~ String ) => GenParser Char st (Header a)
parseDate = do
  date
  spaces
  userDate <- many1 (alphaNum <|> dateSep)
  return $ Date userDate

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
parseMoods' :: GenParser Char st [(String,String)]
parseMoods' = do
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


-- parses the sleep header and it's data
parseSleep :: forall a a' st. (a ~ WakeUp, a' ~ Sleep) => GenParser Char st (Header (a,a'))
parseSleep = do
  sleep
  eol1
  (string "Wake up :" <* spaces) <|> (string "wake up :" <* spaces)
  wakeUpTime <- time
  eol1
  (string "Sleep :" <* spaces) <|> (string "sleep :" <* spaces)
  sleepTime <- time
  return $ Sleep (wakeUpTime, sleepTime)


--parses the productivity header
-- alcohol header
alcohol :: GenParser Char st String
alcohol = header "Alcohol" <|> header "alcohol"


parseAlcohol :: forall a st. GenParser Char st (Header (a,a))
parseAlcohol = undefined
--   header
--   eol1
--   many1 (many1 alphaNum )

cigarette :: GenParser Char st String
cigarette = header "Cigarette" <|> header "cigarette"


parseCigarette :: forall a st. (a ~ String) => GenParser Char st (Header (a,a,a))
parseCigarette = undefined


meditation :: GenParser Char st String
meditation = header "Meditation" <|> header "Meditation"

parseMeditation :: forall a st. (a ~ String) => GenParser Char st (Header [a])
parseMeditation = undefined

productivity :: GenParser Char st String
productivity = header "Productivity" <|> header "productivity"

parseProductivity :: forall a st. (a ~ String) => GenParser Char st (Header (a,a))
parseProductivity = do
  productivity
  eol1
  done <- many1 digit <* char '/'
  shouldHave <- many1 digit
  return $ Productivity (done,shouldHave)


-- parses the rating header
rating :: GenParser Char st String
rating = header "Rating" <|> header "rating"


-- convRating :: GenParser Char st String -> Header a
-- convRating

-- parses the different rating a user might give
parseRating' :: GenParser Char st String
parseRating' = string "Great"
           <|> string "Good"
           <|> string "Neutral"
           <|> string "Bad"
           <|> string "Awful"

-- parses the whole Rating header (section)
parseRating :: forall a st. (a ~ String) => GenParser Char st (Header a)
parseRating = do
  rating
  eol1
  prsd <- parseRating'
  return $ Rating prsd


-- parses the Entry written by the user
parseEntry :: IO T.Text -> [Header String]
parseEntry = undefined
