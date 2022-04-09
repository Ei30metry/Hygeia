module Template where

import           Computation
import qualified Data.Text              as T
import           Data.Time
import           Data.Time.Clock.System
import           Data.Time.Format


generateTemplate :: FilePath -> IO ()
generateTemplate path = do
  writeFile path $ show Date
  time <- getCurrentTime
  appendFile path $ show time
  appendFile path $ "\n" ++ (mconcat . map show)  [Name, MoodH, Productivity, Meditation, Sleep, Rating]


generateTemplateForMonth :: Filepath -> IO ()
generateTemplateForMonth = undefined

generateTemplateForYear :: Filepath -> IO ()
generateTemplateForYear = undefined
