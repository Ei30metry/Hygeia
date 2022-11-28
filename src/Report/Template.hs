module Report.Template where

import           Control.Monad.Trans.Cont

import           Data.Functor
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO
import qualified Data.Time                     as TI

import qualified Parser.Input                        as P

import           System.Environment
import           System.IO

import           Text.ParserCombinators.Parsec


data TemplateHeaders where
  NameT :: String -> TemplateHeaders
  DateT  :: forall t. t ~ TI.UTCTime => t -> TemplateHeaders
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
  show (DateT t)     = "Date : " ++ show (TI.utctDay t)
  show MoodT         = generateHeader "Mood"
  show ProductivityT = generateHeader "ProductivityT"
  show MeditationT   = generateHeader "Meditation"
  show AlcoholT      = generateHeader "Alcohol"
  show CigaretteT    = generateHeader "Cigarette"
  show RatingT       = generateHeader "Rating"


-- generates templates for user to fill in
writeTemplate :: IO ()
writeTemplate = undefined --do
  -- date <- DateT <$> TI.getCurrentTime
  -- configFile <- readFile "~/.config/Hygeia/config"
  -- name <- NameT <$> parse P.parseInfo "couldn't parse name" configFile
  -- entryFile <- openFile "~/.Hygeia/Entries/Entry-1.Hygeia" WriteMode
  -- hClose entryFile
  -- return ()
