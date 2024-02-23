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


unsafeAddMood :: Mood -> Mood -> Mood
unsafeAddMood = undefined


addMood :: Mood -> Mood -> Maybe Mood
addMood x y
  | sameMood x y = Just $ unsafeAddMood x y
  | otherwise = Nothing

-- Rating data type for rating the day
data Rating = Awful
            | Bad
            | Normal
            | Good
            | Great
            deriving (Show, Eq, Ord, Enum, Read, Bounded)


-- safe function to combine two moods
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


condenseDrinks :: Drinks -> Drinks
condenseDrinks = coerce . map (foldr unsafeCombineDrink (Drink "" 0)) . groupBy sameDrink . coerce


condenseCigarettes :: Cigarettes -> Cigarettes
condenseCigarettes = coerce . map (foldr unsafeCombineCigarette (Cigarette "" 0 0 0)) . groupBy sameCigarette . coerce


type Name = String


data Drink = Drink { drinkName :: String
                   , shots :: Int } deriving (Eq, Ord, Show)


newtype Drinks = Drinks [Drink] deriving (Show, Eq)


sameDrink :: Drink -> Drink -> Bool
sameDrink (Drink d _) (Drink d' _) = d == d'


unsafeCombineDrink :: Drink -> Drink -> Drink
unsafeCombineDrink (Drink d s) (Drink _ s') = Drink d (s + s')


combineDrink :: Drink -> Drink -> Maybe Drink
combineDrink a1 a2
  | sameDrink a1 a2 = Just $ unsafeCombineDrink a1 a2
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


unsafeCombineCigarette :: Cigarette -> Cigarette -> Cigarette
unsafeCombineCigarette (Cigarette c ns n t) (Cigarette _ ns' _ _) = Cigarette c (ns + ns') n t


combineCigarette :: Cigarette -> Cigarette -> Maybe Cigarette
combineCigarette c1 c2
  | sameCigarette c1 c2 = Just $ unsafeCombineCigarette c1 c2
  | otherwise = Nothing


data Stage = Parsed | Summaraized deriving (Show, Eq)

-- TODO Change the name to something else
type family UnList a where
  UnList [Day]          = [Day]
  UnList (Entry Parsed) = Entry Parsed
  UnList [a]            = a
  UnList a              = a


type family SummaryType a where
  SummaryType Moods               = Moods
  SummaryType Day                 = Day
  SummaryType Rating              = Rating
  SummaryType Sleep               = Sleep
  SummaryType Drinks              = Drinks
  SummaryType Productivity        = Productivity
  SummaryType Cigarettes          = Cigarettes
  SummaryType Meditations         = Meditations
  SummaryType [Day]               = [Day]
  SummaryType [Moods]             = ([Moods],Moods)
  SummaryType [Sleep]             = ([Sleep],Sleep)
  SummaryType [Drinks]            = ([Drinks],Drinks)
  SummaryType [Rating]            = ([Rating],Rating)
  SummaryType [Productivity]      = ([Productivity],Productivity)
  SummaryType [Cigarettes]        = ([Cigarettes],Cigarettes)
  SummaryType [Meditations]       = ([Meditations],Meditations)
  SummaryType (Entry Parsed)      = Entry Parsed


type family XXDay a where
  XXDay Parsed      = Day
  XXDay Summaraized = SummaryType Day


type family XXMoods a where
  XXMoods Parsed      = Moods
  XXMoods Summaraized = SummaryType Moods


type family XXProductivity a where
  XXProductivity Parsed      = Productivity
  XXProductivity Summaraized = SummaryType Productivity


type family XXMeditations a where
  XXMeditations Parsed      = Meditations
  XXMeditations Summaraized = SummaryType Meditations


type family XXSleep a where
  XXSleep Parsed      = Sleep
  XXSleep Summaraized = SummaryType Sleep


type family XXDrinks a where
  XXDrinks Parsed      = Drinks
  XXDrinks Summaraized = SummaryType Drinks


type family XXCigarettes a where
  XXCigarettes Parsed      = Cigarettes
  XXCigarettes Summaraized = SummaryType Cigarettes


type family XXRating a where
  XXRating Parsed      = Rating
  XXRating Summaraized = SummaryType Rating


data Entry a = Entry { entryDay          :: !(XXDay a)
                     , entryMoods        :: !(XXMoods a)
                     , entrySleep        :: !(XXSleep a)
                     , entryProductivity :: !(XXProductivity a)
                     , entryMeditations  :: !(XXMeditations a)
                     , entryDrinks       :: !(XXDrinks a)
                     , entryCigarettes   :: !(XXCigarettes a)
                     , entryRating       :: !(XXRating a) }
