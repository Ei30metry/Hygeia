module Template () where

import           Config

import           Control.Applicative        ( Alternative )
import           Control.Lens.Operators     ( (^.) )
import           Control.Monad              ( guard )
import           Control.Monad.IO.Class     ( MonadIO, liftIO )
import           Control.Monad.Trans.Reader ( ReaderT (..), ask )

import           Data.ByteString.Char8      ( ByteString )
import qualified Data.ByteString.Char8      as B
import           Data.List                  ( sort )
import qualified Data.Text                  as T
import qualified Data.Text.IO               as TIO
import           Data.Time                  ( Day, UTCTime, getCurrentTime,
                                              utctDay )

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
createTemplate :: FilePath -> ByteString -> Day -> OptHeader -> (FilePath,ByteString)
createTemplate entryPath name date optHeaders = (templatePath, entry)
  where
   optHeadersToInclude = optionalHeadersToGenerate optHeaders
   templatePath = mconcat [entryPath <> "/.Hygeia/", show date, ".entry"]
   entry = mconcat $ map (B.pack . show) headers
   headers = sort $ [ NameT name, DateT date
                        , MoodT, SleepT, ProductivityT, RatingT ] ++ optHeadersToInclude

-- | generates an entry file given a config
generateTemplateFile :: (Alternative m , MonadIO m) => UTCTime -> ReaderT Config m ()
generateTemplateFile time = do
  conf <- ask
  let write = conf ^. entryConf . templateConf . genTemplate
  guard write
  let userName = conf ^. userInfo . name
      optHeaders = conf ^. entryConf . templateConf . optionalHeaders
      curDate = utctDay time
      entryPath = conf ^. entryConf . entryDirectory
      (path, content) = (createTemplate entryPath userName curDate optHeaders)
  liftIO $ B.writeFile path content
