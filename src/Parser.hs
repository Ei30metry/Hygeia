{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Parser where

import qualified Data.Text                           as T
import qualified Data.Text.IO                        as TIO
import           Text.ParserCombinators.Parsec
import           Text.ParserCombinators.Parsec.Token

-- parses '\n' charecters
eol1 :: GenParser Char st String
eol1 = many (char '\n')

time :: GenParser Char st String
time = many1 digit <> many1 (char ':') <> many1 digit

header :: String -> GenParser Char st String
header h = string' $ mconcat ["[", h, "]"]

-- parses the name section
name :: GenParser Char st String
name = string' "Name :" <|> string' "Name: "

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
date = string' "Date :" <|> string' "date :" <|> string' "Date:" <|> string' "date:"

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

parseMood :: GenParser Char st [[String]]
parseMood = undefined

-- parseMood :: GenParser Char st String
-- parseMood = string "Neutral"
--         <|> string "Angry"
--         <|> string "Sad"
--         <|> string "Happy"
--         <|> string "Bored"
--         <|> string "Focused"
--         <|> string "Stressed"
--         <|> string "Excited"

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

parseRating :: GenParser Char st String
parseRating = undefined

-- implement this with Parsec
-- parseFrac :: String -> (String, String)
-- parseFrac frac | length frac == 3 = toTuple . map T.unpack . T.splitOn "/" $ T.pack frac
--                | otherwise = ("","")
--             where toTuple :: [a] -> (a,a)
--                   toTuple (x:xs) = (x, unList xs)
--                   unList :: [a] -> a
--                   unList [x] = x


-- parse :: String -> HList '[Name, Date, MoodReport, Sleep, Productivity, Rating]
-- parse = undefined
