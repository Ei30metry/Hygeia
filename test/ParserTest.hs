-- | Unit test for all of the parser modules
{-# LANGUAGE OverloadedStrings #-}

module ParserTest where

import           Computation.Types
import           Computation.Utils

import           Data.ByteString.Char8 ( ByteString, pack, unpack )
import qualified Data.ByteString.Char8 as B
import           Data.Ratio
import           Data.Time             ( Day (..) )

import           Parser.Entry
import           Parser.Monad
import           Parser.Types

import           Test.Tasty.HUnit


productivityHeaderTest :: Assertion
productivityHeaderTest
  = assertEqual "Productivity header"
                (Right . HProductivity $ Pro (6 % 11))
                (runParser parseProductivity sampleProductivityHeader)
  where sampleProductivityHeader = "[Productivity]\n\n6/11\n"


-- TODO assertion test for not-acceptable values, like negative days, bigger than 12 months and ..
dayHeaderTest :: Assertion
dayHeaderTest = assertEqual "Day header"
                            (Right $ read @Day "2022-04-04")
                            (runParser parseDay sampleDay)
  where sampleDay = "Date : 2022-04-04\n"


moodHeaderTest :: Assertion
moodHeaderTest = assertEqual "Mood header"
                             (Right . HMoods $ [Neutral, Angry High
                                               ,Neutral, Sad Low
                                               ,Neutral, Happy Extreme])
                             (runParser parseMoods sampleMoodHeader)

  where
    sampleMoodHeader = "[Mood]\n\nNeutral\nAngry : High\nNeutral\n\nSad : Low\nNeutral\nHappy : Extreme\n"


parseTimeTest :: Assertion
parseTimeTest = do
  assertEqual "Parse time 06:30" (Right 23400) (runParser time "06:30")
  assertEqual "Parse time 00:00" (Right 0) (runParser time "00:00")
  assertEqual "Parse time 13:45" (Right 49500) (runParser time "13:45")
  assertBool "Won't Parse time 25:55" $ (runParser time "25:55") /= (Right 93300)
  assertBool "Won't Parse negative time" $ (runParser time "-10:55") /= (Right (-32700))


sleepHeaderTest :: Assertion
sleepHeaderTest = assertEqual "Sleep header"
                              (Right . HSleep $ SP 23400 0)
                              (runParser parseSleep sampleSleepHeader)
  where
    sampleSleepHeader = "[Sleep]\n\nWake up : 06:30\nSleep : 00:00\n\n"


drinkHeaderTest :: Assertion
drinkHeaderTest = assertEqual "Drink header"
                              (Right . HDrinks $ [Drink "tequila" 5
                                                 ,Drink "Aragh" 10])
                              (runParser parseDrinks sampleDrinkHeader)

  where
    sampleDrinkHeader = "[Drink]\n\ntequila : 5\n\n\n\n\nAragh : 10\n"


meditationHeaderTest :: Assertion
meditationHeaderTest = assertEqual "Meditation header"
                                   (Right . HMeditations $ [Med ("20",1200)
                                                           ,Med ("25",1500)
                                                           ,Med ("15",900)])
                                   (runParser parseMeditations sampleMeditationHeader)
  where
    sampleMeditationHeader = "[Meditation]\n\n20\n25\n15\n\n"


cigaretteHeaderTest :: Assertion
cigaretteHeaderTest = assertEqual "Cigarette header"
                     (Right . HCigarettes $ [Cigarette "Cigarettes are bad" 7 0.6 0.7
                                            ,Cigarette "Cigarettes are terrible" 5 1 1.2])
                     (runParser parseCigarettes sampleCigaretteHeader)

  where
    sampleCigaretteHeader = "[Cigarette]\nName : Cigarettes are bad\nNumber : 7\nNicotine : 0.6\nTar : 0.7\n\nName : Cigarettes are terrible\nNumber : 5\nNicotine : 1\nTar : 1.2\n"


ratingHeaderTest :: Assertion
ratingHeaderTest = assertEqual "Rating header"
                                (Right . HRating $ Bad)
                                (runParser parseRating "[Rating]\nBad")

-- NOTE Turn this into an assertion
parseEntryTest :: Assertion
parseEntryTest = do
  sample <- B.readFile "/Users/artin/Programming/projects/Hygeia/test/sample.txt"
  case runParser parseEntry sample of
    Right x -> return () 
    Left y  -> putStrLn y

