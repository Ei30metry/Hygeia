{-# LANGUAGE TupleSections #-}
-- |
module Computation.Types where

import           Computation.Utils

import           Control.Monad     ( foldM, join, (<=<), (=<<) )

import           Data.Coerce
import           Data.Foldable     ( toList )
import           Data.List
import           Data.Maybe
import           Data.Ratio
import           Data.Time         ( Day, DiffTime, defaultTimeLocale,
                                     formatTime, secondsToDiffTime )

import           Text.Read         ( readEither )


data Mood = Angry Intensity
          | Sad Intensity
          | Neutral
          | Happy Intensity
          | Excited Intensity
          deriving (Read, Eq, Ord, Show)


newtype Moods = Moods { unMoods :: [Mood] } deriving (Eq, Ord, Show)

-- | Intensity of a mood
data Intensity = None      -- Only here because of mempty
               | Low
               | Medium
               | High
               | Extreme
               deriving (Show, Read, Eq, Ord, Enum, Bounded)


computeIntensity :: Intensity -> Intensity -> Intensity
computeIntensity x y | fromEnum x == fromEnum y = x
                     | fromEnum x > fromEnum y = x
                     | otherwise = y


instance Semigroup Intensity where
  x <> y = computeIntensity x y


instance Monoid Intensity where
  mappend = (<>)
  mempty = None


sameMood :: Mood -> Mood -> Bool
sameMood (Angry _) (Angry _)     = True
sameMood (Sad _) (Sad _)         = True
sameMood Neutral Neutral         = True
sameMood (Happy _) (Happy _)     = True
sameMood (Excited _) (Excited _) = True
sameMood _ _                     = False

-- Rating data type for rating the day
data Rating = Awful
            | Bad
            | Normal
            | Good
            | Great
            deriving (Show, Eq, Ord, Enum, Read, Bounded)


-- safe function to add MoodReports
combineMoods :: Mood -> Mood -> Maybe Mood
combineMoods (Angry x) (Angry y)     = Just $ Angry (x <> y)
combineMoods (Sad x) (Sad y)         = Just $ Sad (x <> y)
combineMoods Neutral Neutral         = Just Neutral
combineMoods (Happy x) (Happy y)     = Just $ Happy (x <> y)
combineMoods (Excited x) (Excited y) = Just $ Excited (x <> y)
combineMoods _ _                     = Nothing


condenseMoods :: Moods -> Moods
condenseMoods = coerce . fmap go . groupBy sameMood . sort . coerce
  where
    x <!> y = fromJust (combineMoods x y)
    go xs
      | length xs == 1 = head xs
      | otherwise = foldr (<!>) (head xs) xs


type Name = String


data Alcohol = Alcohol { drink :: String
                       , shots :: Int } deriving (Eq, Ord, Show)


newtype Drinks = Drinks [Alcohol] deriving (Show, Eq)


sameDrink :: Alcohol -> Alcohol -> Bool
sameDrink (Alcohol d _) (Alcohol d' _) = d == d'


unsafeAddShots :: Alcohol -> Alcohol -> Alcohol
unsafeAddShots (Alcohol d s) (Alcohol _ s') = Alcohol d (s + s')


addShots :: Alcohol -> Alcohol -> Maybe Alcohol
addShots a1 a2
  | sameDrink a1 a2 = Just $ unsafeAddShots a1 a2
  | otherwise = Nothing


data Sleep = SP { wakeUpTime :: DiffTime
                , sleepTime  :: DiffTime } deriving (Eq, Ord)


instance Show Sleep where
  show (SP w s) = mconcat ["wake up: ",formatTime defaultTimeLocale "%H:%M" w,"\n"
                          ,"Sleep: ",formatTime defaultTimeLocale "%H:%M" s]


newtype Meditation = Med { unMed :: (String, DiffTime) } deriving (Eq, Ord)


mkMeditaiton :: String -> Either String Meditation
mkMeditaiton minutes = Med . (minutes,)
                    <$> (return . secondsToDiffTime . (60*)
                          =<< readEither @Integer minutes)


newtype Meditations = Meds [Meditation] deriving (Eq, Show, Semigroup, Monoid)


instance Semigroup Meditation where
  Med (a,b) <> Med (c,d) = Med (a <> "," <> c, b + d)


instance Monoid Meditation where
  mempty = Med ("", secondsToDiffTime 0)
  mappend = (<>)


newtype Productivity = Pro { unPro :: Rational } deriving (Eq, Ord)


instance Show Productivity where
  show (Pro a) = mconcat [show (numerator a) ,"/",show (denominator a)]


instance Semigroup Productivity where
  (Pro a) <> (Pro b) = Pro $ a + b


instance Monoid Productivity where
  mappend = (<>)
  mempty  = Pro 0


instance Show Meditation where
  show (Med a) = show a


data Cigarette = Cigarette { cigaretteName :: String
                           , numberSmoked  :: Double
                           , nicotine      :: Double
                           , tar           :: Double }
                           deriving (Eq, Ord, Show)

newtype Cigarettes = Cigarettes [Cigarette] deriving (Show, Eq, Ord)

sameCigarette :: Cigarette -> Cigarette -> Bool
sameCigarette (Cigarette c _ _ _) (Cigarette c' _ _ _) = c == c'


unsafeAddSmokes :: Cigarette -> Cigarette -> Cigarette
unsafeAddSmokes (Cigarette c ns n t) (Cigarette _ ns' _ _) = Cigarette c (ns + ns') n t


addSmokes :: Cigarette -> Cigarette -> Maybe Cigarette
addSmokes c1 c2
  | sameCigarette c1 c2 = Just $ unsafeAddSmokes c1 c2
  | otherwise = Nothing


data Stage = Parser | Summaraizer deriving (Show, Eq)

-- TODO Change the name to something else
type family UnList a where
  UnList [Day]          = [Day]
  UnList (Entry Parser) = Entry Parser
  UnList [a]            = a
  UnList a              = a


type family SummaryType a | a -> a where
  SummaryType [Day]          = [Day]
  SummaryType Moods          = ([Moods],Moods)
  SummaryType Sleep          = ([Sleep],Sleep)
  SummaryType Drinks         = ([Drinks],Drinks)
  SummaryType Rating         = ([Rating],Rating)
  SummaryType Productivity   = ([Productivity],Productivity)
  SummaryType Cigarettes     = ([Cigarettes],Cigarettes)
  SummaryType Meditations    = ([Meditations],Meditations)
  SummaryType (Entry Parser) = Entry Summaraizer


type family XXDay a where
  XXDay Parser      = Day
  XXDay Summaraizer = SummaryType Day


type family XXMoods a where
  XXMoods Parser      = Moods
  XXMoods Summaraizer = SummaryType Moods


type family XXProductivity a where
  XXProductivity Parser      = Productivity
  XXProductivity Summaraizer = SummaryType Productivity


type family XXMeditations a where
  XXMeditations Parser      = Meditations
  XXMeditations Summaraizer = SummaryType Meditations


type family XXSleep a where
  XXSleep Parser      = Sleep
  XXSleep Summaraizer = SummaryType Sleep


type family XXDrinks a where
  XXDrinks Parser      = Drinks
  XXDrinks Summaraizer = SummaryType Drinks


type family XXCigarettes a where
  XXCigarettes Parser      = Cigarettes
  XXCigarettes Summaraizer = SummaryType Cigarettes


type family XXRating a where
  XXRating Parser      = Rating
  XXRating Summaraizer = SummaryType Rating


data Entry a = Entry { entryDay          :: XXDay a
                     , entryMoods        :: XXMoods a
                     , entrySleep        :: XXSleep a
                     , entryProductivity :: XXProductivity a
                     , entryMeditations  :: XXMeditations a
                     , entryDrinks       :: XXDrinks a
                     , entryCigarettes   :: XXCigarettes a
                     , entryRating       :: XXRating a }
