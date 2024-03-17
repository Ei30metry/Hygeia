module Template where

import           Config

import           Control.Applicative        ( Alternative )
import           Control.Lens.Operators     ( (^.) )
import           Control.Monad
import           Control.Monad.IO.Class     ( MonadIO, liftIO )
import           Control.Monad.Trans.Reader ( ReaderT (..), ask )

import           Data.ByteString.Char8      ( ByteString )
import qualified Data.ByteString.Char8      as B
import           Data.List                  ( sort )
import qualified Data.Text                  as T
import qualified Data.Text.IO               as TIO
import           Data.Time                  ( Day, UTCTime, getCurrentTime,
                                              utctDay )
import           Data.Traversable

import           System.Directory           ( getHomeDirectory )
import           System.IO                  ( writeFile )


data TemplateHeaders = NameT ByteString
                     | DateT Day
                     | MoodT
                     | SleepT
                     | MeditationT
                     | DrinkT
                     | CigaretteT
                     | ProductivityT
                     | RatingT deriving (Eq, Ord)



generateHeader "Rating" = mconcat ["[","Rating","]","\n"]
generateHeader header   = mconcat ["[",header,"]","\n\n"]


instance Show TemplateHeaders where
  show (NameT a)     = "Name : " ++ B.unpack a ++ "\n"
  show (DateT t)     = "Date : " ++ show t ++ "\n\n"
  show MoodT         = generateHeader "Mood"
  show ProductivityT = generateHeader "Productivity"
  show MeditationT   = generateHeader "Meditation"
  show DrinkT        = generateHeader "Drink"
  show SleepT        = generateHeader "Sleep"
  show CigaretteT    = generateHeader "Cigarette"
  show RatingT       = generateHeader "Rating"

-- | Generates a list of Optional Headers based on the configuration
optionalHeadersToGenerate :: OptHeader -> [TemplateHeaders]
optionalHeadersToGenerate (OptH m a c) = [fst x | x <- zip [MeditationT, DrinkT, CigaretteT] [m, a, c], snd x]

-- | Writes the template entry file to a file with the date as its name
createTemplate :: FilePath -> ByteString -> Day -> OptHeader -> ByteString
createTemplate entryPath name date optHeaders = mconcat $ map (B.pack . show) headers
  where
    optHeadersToInclude = optionalHeadersToGenerate optHeaders
    headers = [ NameT name, DateT date
              , MoodT, SleepT ]
           ++ optHeadersToInclude
           ++ [ProductivityT, RatingT]

-- NOTE The entryPath directory should exist.
-- | Generates a template entry file given a config
writeTemplate :: (Alternative m , MonadIO m) => Day -> ReaderT Config m ()
writeTemplate date = do
  conf <- ask
  let write = conf ^. templateConf . genTemplate
  guard write
  let userName = B.pack $ conf ^. userInfo . name
      optHeaders = conf ^. optionalHeaders
      entryPath = conf ^. entryDirectory
      path = mconcat [entryPath, show date, ".entry"]
  liftIO $ B.writeFile path (createTemplate entryPath userName date optHeaders)


writeTemplates :: (MonadIO m, Alternative m) => Interval -> Day -> ReaderT Config m ()
writeTemplates interval firstDay = do
  today <- utctDay <$> liftIO getCurrentTime
  case buildDays interval firstDay today of
    Just days -> mapM_ writeTemplate days
    Nothing -> liftIO $ putStrLn "Something wen't wrong"
