{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs               #-}

module Template where

import qualified Config       as C
import qualified Data.Text    as T
import qualified Data.Text.IO as TIO
import qualified Data.Time    as TI


data TemplateHeaders where
  NameT :: String -> TemplateHeaders
  DateT  :: TI.UTCTime -> TemplateHeaders
  MoodT :: TemplateHeaders
  ProductivityT :: TemplateHeaders
  MeditationT :: TemplateHeaders
  AlcoholT :: TemplateHeaders
  CigaretteT :: TemplateHeaders
  RatingT :: TemplateHeaders


generateHeader :: String -> String
generateHeader header = mconcat ["[",header,"]","\n\n"]


instance Show TemplateHeaders where
  show (NameT a)     = "Name : " ++ a
  show (DateT a)     = "Date : " ++ show a
  show MoodT         = generateHeader "Mood"
  show ProductivityT = generateHeader "ProductivityT"
  show MeditationT   = generateHeader "Meditation"
  show AlcoholT      = generateHeader "Alcohol"
  show CigaretteT    = generateHeader "Cigarette"
  show RatingT       = generateHeader "Rating"



generateTemplate :: IO ()
generateTemplate = undefined
