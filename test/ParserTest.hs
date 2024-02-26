-- |
{-# LANGUAGE OverloadedStrings #-}

module ParserTest where

import           Computation.Types

import           Data.ByteString.Char8 ( ByteString )
import           Data.Ratio
import           Data.Time             ( Day (..) )

import           Parser.Entry
import           Parser.Monad
import           Parser.Types

import           Test.Tasty.HUnit


productivityHeaderTest :: Assertion
productivityHeaderTest
  = assertEqual "Productivity header"
               (runParser parseProductivity sampleProductivityHeader)
               (Right . HProductivity $ Pro (6 % 11))
  where sampleProductivityHeader = "[Productivity]\n\n6/11\n"


dayHeaderTest :: Assertion
dayHeaderTest = assertEqual "Day header" (runParser parseDay sampleDay) (Right $ read @Day "2022-04-04")
  where sampleDay = "Date : 2022-04-04\n"


moodHeaderTest :: Assertion
moodHeaderTest = assertEqual "Mood header" (runParser parseMoods sampleMoodHeader)
                 (Right . HMoods $ [Neutral, Angry High
                                   ,Neutral, Sad Low
                                   ,Neutral, Happy Extreme])
  where
    sampleMoodHeader = "[Mood]\n\nNeutral\nAngry : High\nNeutral\n\nSad : Low\nNeutral\nHappy : Extreme\n"

parseTimeTest :: Assertion
parseTimeTest = do
  assertEqual "Parse time 06:30" (runParser time "06:30") (Right 23400)
  assertEqual "Parse time 00:00" (runParser time "00:00") (Right 0)
  assertEqual "Parse time 13:45" (runParser time "13:45") (Right 49500)
  assertBool  "Won't Parse time 25:55" $ (runParser time "25:55") /= (Right 93300)

sleepHeaderTest :: Assertion
sleepHeaderTest = assertEqual "Sleep header" (runParser parseSleep sampleSleepHeader)
                  (Right . HSleep $ SP 23400 0)
  where
    sampleSleepHeader = "[Sleep]\n\nWake up : 06:30\nSleep : 00:00\n\n"
    

drinkHeaderTest :: Assertion
drinkHeaderTest = assertEqual "Drink header" (runParser parseDrinks sampleDrinkHeader)
                 (Right . HDrinks $ [Drink "tequila" 5
                                    ,Drink "Aragh" 10])
  where
    sampleDrinkHeader = "[Drink]\n\ntequila : 5\n\n\n\n\naragh : 10\n"


meditationHeaderTest :: Assertion
meditationHeaderTest = assertEqual "Meditation header" (runParser parseDrinks sampleMeditationHeader)
                       (Right . HMeditations $ [Med ("20", 1200)
                                               ,Med ("25",1500)
                                               ,Med ("15",900)])
  where
    sampleMeditationHeader = "[Meditation]\n\n20\n25\n15\n\n"


cigaretteHeaderTest :: Assertion
cigaretteHeaderTest = assertEqual "Cigarette header" (runParser parseCigarettes sampleCigaretteHeader)
                      (Right . HCigarettes $ [Cigarette "Cigarettes are bad" 7 0.6 0.7])
  where
    sampleCigaretteHeader = "[Cigarette]\n\nName : Cigarettes are bad\nNumber : 7\nNicotine : 0.6\nTar : 0.7\n"


ratingHeaderTest :: Assertion
ratingHeaderTest = assertEqual "Rating header" (runParser parseRating "[Rating]\nBad")
                   (Right . HRating $ Bad)
