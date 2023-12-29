module Template (generateTemplateFile) where

import           Config

import           Control.Lens.Operators     ( (^.) )
import           Control.Monad              ( when )
import           Control.Monad.IO.Class     ( liftIO )
import           Control.Monad.Trans.Reader ( ReaderT (..), ask )

import           Data.ByteString.Char8      ( ByteString )
import qualified Data.ByteString.Char8      as B
import           Data.List                  ( sort )
import qualified Data.Text                  as T
import qualified Data.Text.IO               as TIO
import           Data.Time                  ( Day, getCurrentTime, utctDay )

import           System.Directory           ( getHomeDirectory )
import           System.IO                  ( writeFile )

data TemplateHeaders = NameT ByteString
                     | DateT Day
                     | MoodT
                     | SleepT
                     | MeditationT
                     | AlcoholT
                     | CigaretteT
                     | ProductivityT
                     | RatingT deriving (Eq, Ord)


generateHeader :: String -> String
generateHeader "Rating" = mconcat ["[","Rating","]"]
generateHeader header   = mconcat ["[",header,"]","\n\n"]


instance Show TemplateHeaders where
  show (NameT a)     = "Name : " ++ B.unpack a ++ "\n"
  show (DateT t)     = "Date : " ++ show t ++ "\n\n"
  show MoodT         = generateHeader "Mood"
  show ProductivityT = generateHeader "ProductivityT"
  show MeditationT   = generateHeader "Meditation"
  show AlcoholT      = generateHeader "Alcohol"
  show SleepT        = generateHeader "Sleep"
  show CigaretteT    = generateHeader "Cigarette"
  show RatingT       = generateHeader "Rating"


-- generates a list of Optional Headers based on the configuration
optionalHeadersToGenerate :: OptHeader -> [TemplateHeaders]
optionalHeadersToGenerate (OptH m a c) = [fst x | x <- zip [MeditationT, AlcoholT, CigaretteT] [m, a, c], snd x]

-- write the template entry file to a file with the date as its name
writeTemplate :: ByteString -> Day -> OptHeader -> IO ()
writeTemplate name date optHeaders = do
   let optHeadersToInclude = optionalHeadersToGenerate optHeaders
   let headers = sort $ [ NameT name, DateT date
                        , MoodT, SleepT, ProductivityT, RatingT ] ++ optHeadersToInclude
   homeDir <- getHomeDirectory
   B.writeFile (mconcat [homeDir <> "/.Hygeia/", show date, ".entry"]) $ mconcat $ map (B.pack . show) headers


-- generates an entry file given a config
generateTemplateFile :: ReaderT Config IO ()
generateTemplateFile = do
  conf <- ask
  let write = conf ^. template . genTemplate . generateTemplate
  let userName = conf ^. info . name
  let optHeaders = conf ^. template . optionalHeaders
  curDate <- utctDay <$> liftIO getCurrentTime
  when write $ liftIO (writeTemplate userName curDate optHeaders)
