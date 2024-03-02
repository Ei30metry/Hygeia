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

import           Test.Hspec
import           Test.Tasty.Hspec


spec_productivityHeader :: Spec
spec_productivityHeader = describe "Productivity header" $ do
  it "Parses the productivity header" $
     (runParseProductivity sample1)
     `shouldBe`
     (Right . HProductivity $ Pro (6 % 11))

  it "Throws an error if the denominator is 0" $
      (runParseProductivity sample2)
      `shouldBe`
      Left (show DivisionByZero)

  it "Throws an error if the numerator is bigger than the denominator" $
      (runParseProductivity sample3)
      `shouldBe`
      (Left . show . TooProductive . show $ Pro (10 % 9))
  where
    runParseProductivity = runParser parseProductivity
    sample1 = "[Productivity]\n\n6/11\n"
    sample2 = "[Productivity]\n5/0\n"
    sample3 = "[Productivity]\n10/9\n"

-- TODO assertion test for not-acceptable values, like negative days, bigger than 12 months and ..
spec_dayHeader :: Spec
spec_dayHeader = describe "Day header" $ do 
   it "Parses the day header with the date 2022-04-04" $
     (runParser parseDay sample1)
     `shouldBe`
     (Right $ read @Day "2022-04-04")

   it "Parses the day header with the date 2012-12-12" $
     (runParser parseDay sample2)
     `shouldBe`
     (Right $ read @Day "2012-12-12")

   it "Throws Invalid date error on 2023-15-48" $
     (runParser parseDay sample3)
     `shouldBe`
     Left "Not a valid date: 2023-15-48"
  where
    sample1 = "Date : 2022-04-04\n"
    sample2 = "Date : 2012-12-12\n"
    sample3 = "Date : 2023-15-48\n"


spec_moodHeader :: Spec
spec_moodHeader = describe "Mood header" $ do 
    it "Parses correct mood entries" $ 
        (runParser parseMoods sample1)
        `shouldBe`
        result1
    it "Throws Unknown Intensity error on wrong intensity" $
        (runParser parseMoods sample2)
        `shouldBe`
        result2
    it "Throws Unknown Mood error on wrong mood" $
        (runParser parseMoods sample3)
        `shouldBe`
        result3
  where
    sample1 = "[Mood]\n\nNeutral\nAngry : High\nNeutral\n\nSad : Low\nNeutral\nHappy : Extreme\nNeutral\n\n\n\n\n\n"              -- NOTE Add Neutral : Extreme and update the parser to give the right error message
    sample2 = "[Mood]\nHappy : Blah"
    sample3 = "[Mood]\nBlah : Extreme"
    result1 = Right . HMoods $ [Neutral, Angry High
                              ,Neutral, Sad Low
                              ,Neutral, Happy Extreme
                              ,Neutral]
    result2 = Left . show $ UnknownIntensity "Blah"
    result3 = Left . show $ UnknownMood "Blah"

spec_parseTime :: Spec
spec_parseTime = describe "Time input" $ do
    it "Parses the time 06:30" $
      (runParser time "06:30")
      `shouldBe`
      (Right 23400)
    it "Parses the time 00:00" $
      (runParser time "00:00")
      `shouldBe`
      (Right 0)
    it "Parses the time 13:45" $
      (runParser time "13:45")
      `shouldBe`
      (Right 49500)
    it "Won't parse time 25:55" $
      (runParser time "25:55")
      `shouldBe`
      (Left . show $ InvalidTime "25:55")
    it "Won't parse negative time" $
       (runParser time "-10:55")
       `shouldNotBe`
       (Right (-32700))


spec_sleepHeader :: Spec
spec_sleepHeader = describe "Sleep header" $ do
    it "Parses the correct Sleep header" $ 
       (runParser parseSleep sampleSleepHeader)
       `shouldBe`
       (Right . HSleep $ SP 23400 0)

  where
    sampleSleepHeader = "[Sleep]\n\nWake up : 06:30\nSleep : 00:00\n\n"


spec_drinkHeader :: Spec
spec_drinkHeader = describe "Drink header" $ do
  it "Parses the correct Drink header" $ 
      (runParser parseDrinks sampleDrinkHeader)
      `shouldBe`
      (Right . HDrinks $ [Drink "tequila" 5
                         ,Drink "whiskey" 10])

  where
    sampleDrinkHeader = "[Drink]\n\ntequila : 5\n\n\n\n\nwhiskey : 10\n"


spec_meditationHeader :: Spec
spec_meditationHeader = describe "Meditation header" $
  it "Parses the correct Meditation header" $ 
     (runParser parseMeditations sampleMeditationHeader)
     `shouldBe`
     (Right . HMeditations $ [Med ("20",1200)
                             ,Med ("25",1500)
                             ,Med ("15",900)])
  where
    sampleMeditationHeader = "[Meditation]\n\n20\n25\n15\n\n"


spec_cigaretteHeader :: Spec
spec_cigaretteHeader = describe "Cigarette header" $
  it "Parses the correct Cigarette header" $ 
     (runParser parseCigarettes sampleCigaretteHeader)
     `shouldBe`
     (Right . HCigarettes $ [Cigarette "Cigarettes are bad" 7 0.6 0.7
                            ,Cigarette "Cigarettes are terrible" 5 1 1.2])
  where
    sampleCigaretteHeader = "[Cigarette]\nName : Cigarettes are bad\nNumber : 7\nNicotine : 0.6\nTar : 0.7\n\nName : Cigarettes are terrible\nNumber : 5\nNicotine : 1\nTar : 1.2\n"


spec_ratingHeader :: Spec
spec_ratingHeader = describe "Rating header" $ do
  it "Parses the correct Rating header" $
     (runParser parseRating "[Rating]\nBad")
     `shouldBe`
     (Right . HRating $ Bad)

-- NOTE Turn this into an assertion
spec_parseEntry :: IO ()
spec_parseEntry = do
  sample <- B.readFile "/Users/artin/Programming/projects/Hygeia/test/sample/sample.txt"
  case runParser parseEntry sample of
    Right x -> return ()
    Left y  -> putStrLn y

spec_Parser_Entry :: Spec
spec_Parser_Entry = do
  spec_parseTime
  spec_dayHeader
  spec_productivityHeader
  spec_sleepHeader
  spec_drinkHeader
  spec_meditationHeader
  spec_moodHeader
  spec_ratingHeader
  spec_cigaretteHeader
