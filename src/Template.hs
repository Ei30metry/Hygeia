module Template where

import qualified Config       as C
import qualified Data.Text    as T
import qualified Data.Text.IO as TIO
import qualified Data.Time    as TI
import           Parser


-- generateTemplate :: IO ()
-- generateTemplate path = do
--   writeFile path $ show Date
--   time <- TI.getCurrentTime
--   appendFile path $ show time
--   appendFile path $ "\n" ++ (mconcat . map show)  [Name, MoodH, Productivity, Meditation, Sleep, Rating]

generateTemplate :: IO ()
generateTemplate = undefined
