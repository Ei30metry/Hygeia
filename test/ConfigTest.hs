-- | Unit tests for the Config module


module ConfigTest where

import           Config

import qualified Data.ByteString.Lazy as BL
import           Data.Time
import           Data.Time.Calendar
import           Data.YAML

import           Test.Hspec
import           Test.Tasty.Hspec

-- TODO Turn this into an assertion
-- userWrittenConfigTest :: Assertion
-- userWrittenConfigTest = do
--   sampleConfig <- BL.readFile "/Users/artin/Programming/projects/Hygeia/test/sampleconfig"
--   let val = decode1 sampleConfig :: Either (Pos,String) Config
--   print val

spec_findDayTarget :: Spec
spec_findDayTarget = describe "Find the correct day target" $ do
    it "Can decrease 2024-03-17 by one month" $
       findTargetDay interval1 firstDay today
       `shouldBe`
       (Just $ read "2024-02-17")

    it "Can decrease 2024-03-17 by one year" $
       findTargetDay interval2 firstDay today
       `shouldBe`
       (Just $ read "2023-03-17")

    it "Can return today (2024-03-17)" $
       findTargetDay interval3 firstDay today
       `shouldBe`
       (Just $ today)

    it "Can return the date of the first entry ever" $
       findTargetDay interval4 firstDay today
       `shouldBe`
       (Just $ firstDay)

    it "Can decrease 2024-03-17 by 35 days" $
       findTargetDay interval5 firstDay today
       `shouldBe`
       (Just $ read "2024-02-11")
  
    it "Returns Notning on a date earlier than the first entry date" $
      findTargetDay interval6 firstDay today
      `shouldBe`
      Nothing

    it "Can decrease the date by 3 weeks" $
      findTargetDay interval7 firstDay today
      `shouldBe`
      (Just $ read "2024-02-25")
    -- NOTE This should be in spec_interval
    -- it "Transforms (Months -1) to (Months 1))"

  where
    today = read "2024-03-17"
    firstDay = read "2021-06-02"
    interval1 = Months 1
    interval2 = Years 1
    interval3 = DefInterval Today
    interval4 = DefInterval All
    interval5 = Days 35
    interval6 = Years 20
    interval7 = Weeks 3


spec_Interval :: Spec
spec_Interval = undefined
  where
    interval1 = Months (-1)
    interval2 = Years 2
    interval3 = Date (read "2024-02-11")
    interval4 = DefInterval Today
    interval5 = DefInterval All
    interval6 = Weeks 7
    interval7 = Days 53

    
spec_Config :: Spec
spec_Config = do
  spec_findDayTarget
