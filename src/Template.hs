module Template where

import qualified Data.Text              as T
import           Data.Time
import           Data.Time.Clock.System
import           Data.Time.Format

-- data types representing the Mood of our patient from time to time
-- data Mood a = Sad Int
--           | Angry Int
--           | Bored Int
--           | Stressed Int
--           | Happy Int
--           | Focused Int
--           | Excited deriving (Show, Eq, Ord, Read)

-- -- data Mood (a :: Nat) (b :: Nat) where
-- --   Angry :: Int -> Int -> Mood Int Int
-- --   Bored :: Int -> Int -> Mood Int Int



-- -- -- data types representing the rating user gaves to its days
-- data Ratings = Awful
--              | Bad
--              | Neutral
--              | Good
--              | Great deriving (Show, Enum, Eq, Ord, Read)

data Header = MoodH
            | Name
            | Date
            | Sleep
            | Productivity
            | Rating
            | Meditation deriving Eq


-- instance Read Header where

-- instance (a ~ Header) => Show a where
--   show a = "[" ++ show a ++ "]"

instance Show Header where
  show Name = "Artin Ghasivand\n\n"
  show Date = "Date : "
  show MoodH         = "[Mood]\n\n"
  show Productivity = "[Productivity]\n\n"
  show Sleep = mconcat ["[Sleep]", "\n", "\n", "wake up :", "\n", "sleep :", "\n", "\n"]
  show Rating = "[Rating]\n"
  show Meditation = "[Meditation]\n\n"

generateTemplate :: FilePath -> IO ()
generateTemplate path = do
  writeFile path $ show Date
  time <- getCurrentTime
  appendFile path $ show time
  appendFile path $ "\n" ++ (mconcat . map show)  [Name, MoodH, Productivity, Meditation, Sleep, Rating ]
