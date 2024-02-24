{-# LANGUAGE TupleSections #-}
-- |
module Computation.Types where

import           Computation.Utils

import           Control.Monad     ( foldM, join, (<=<), (=<<) )

import           Data.Bifunctor
import           Data.Coerce
import           Data.Foldable
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


-- | Intensity of a mood
data Intensity = None      -- Only here because of neutral
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


unsafeCombineMoods :: Mood -> Mood -> Mood
unsafeCombineMoods (Angry x) (Angry y)     = Angry (x <> y)
unsafeCombineMoods (Sad x) (Sad y)         = Sad (x <> y)
unsafeCombineMoods Neutral Neutral         = Neutral
unsafeCombineMoods (Happy x) (Happy y)     = Happy (x <> y)
unsafeCombineMoods (Excited x) (Excited y) = Excited (x <> y)
unsafeCombineMoods Neutral x               = x
unsafeCombineMoods x Neutral               = x
unsafeCombineMoods _ _                     = undefined


-- Rating data type for rating the day
data Rating = Awful
            | Bad
            | Normal
            | Good
            | Great
            deriving (Show, Eq, Ord, Enum, Read, Bounded)


type Name = String


data Drink = Drink { drinkName :: String
                   , shots     :: Int }
           deriving (Eq, Ord, Show)


sameDrink :: Drink -> Drink -> Bool
sameDrink (Drink d _) (Drink d' _) = d == d'


unsafeCombineDrinks :: Drink -> Drink -> Drink
unsafeCombineDrinks (Drink d s) (Drink _ s') = Drink d (s + s')


combineDrink :: Drink -> Drink -> Maybe Drink
combineDrink a1 a2
  | sameDrink a1 a2 = Just $ unsafeCombineDrinks a1 a2
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


instance Semigroup Meditation where
  Med (a,b) <> Med (c,d) = Med (a <> "," <> c, b + d)


instance Monoid Meditation where
  mempty = neutral
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



sameCigarette :: Cigarette -> Cigarette -> Bool
sameCigarette (Cigarette c _ _ _) (Cigarette c' _ _ _) = c == c'


unsafeCombineCigarettes :: Cigarette -> Cigarette -> Cigarette
unsafeCombineCigarettes (Cigarette c ns n t) (Cigarette _ ns' _ _) = Cigarette c (ns + ns') n t


combineCigarette :: Cigarette -> Cigarette -> Maybe Cigarette
combineCigarette c1 c2
  | sameCigarette c1 c2 = Just $ unsafeCombineCigarettes c1 c2
  | otherwise = Nothing


data Stage = Parsed | Summaraized deriving (Show, Eq)


data Entry a = Entry { entryDay          :: !(XXDay a)
                     , entryMoods        :: !(XXMoods a)
                     , entrySleep        :: !(XXSleep a)
                     , entryProductivity :: !(XXProductivity a)
                     , entryMeditations  :: !(XXMeditations a)
                     , entryDrinks       :: !(XXDrinks a)
                     , entryCigarettes   :: !(XXCigarettes a)
                     , entryRating       :: !(XXRating a) }


type instance SummaryType Day                 = Day
type instance SummaryType Productivity        = Productivity
type instance SummaryType Rating              = Rating
type instance SummaryType Sleep               = Sleep
type instance SummaryType [Cigarette]         = [Cigarette]
type instance SummaryType [Mood]              = [Mood]
type instance SummaryType [Drink]             = [Drink]
type instance SummaryType [Meditation]        = [Meditation]

type instance SummaryType [Day]               = [Day]
type instance SummaryType [Sleep]             = ([Sleep],Sleep)
type instance SummaryType [Rating]            = ([Rating],Rating)
type instance SummaryType [Productivity]      = ([Productivity],Productivity)
type instance SummaryType [[Mood]]            = ([[Mood]],[Mood])
type instance SummaryType [[Drink]]           = ([[Drink]],[Drink])
type instance SummaryType [[Cigarette]]       = ([[Cigarette]],[Cigarette])
type instance SummaryType [[Meditation]]      = ([[Meditation]],[Meditation])

type instance SummaryType (Entry Parsed)      = Entry Summaraized
type instance SummaryType [Entry Summaraized] = Entry Summaraized


type family XXDay a where
  XXDay Parsed      = Day
  XXDay Summaraized = SummaryType [Day]


type family XXMoods a where
  XXMoods Parsed      = [Mood]
  XXMoods Summaraized = SummaryType [[Mood]]


type family XXProductivity a where
  XXProductivity Parsed      = Productivity
  XXProductivity Summaraized = SummaryType [Productivity]


type family XXMeditations a where
  XXMeditations Parsed      = [Meditation]
  XXMeditations Summaraized = SummaryType [[Meditation]]


type family XXSleep a where
  XXSleep Parsed      = Sleep
  XXSleep Summaraized = SummaryType [Sleep]


type family XXDrinks a where
  XXDrinks Parsed      = [Drink]
  XXDrinks Summaraized = SummaryType [[Drink]]


type family XXCigarettes a where
  XXCigarettes Parsed      = [Cigarette]
  XXCigarettes Summaraized = SummaryType [[Cigarette]]


type family XXRating a where
  XXRating Parsed      = Rating
  XXRating Summaraized = SummaryType [Rating]


instance Neutral Sleep where
  neutral = SP (secondsToDiffTime 0) (secondsToDiffTime 0)


instance Neutral Productivity where
  neutral = Pro 0


instance Neutral Drink where
  neutral = Drink "" 0


instance Neutral Cigarette where
  neutral = Cigarette "" 0 0 0


instance Neutral Rating where
  neutral = Normal


instance Neutral Mood where
  neutral = Neutral


instance Neutral Meditation where
  neutral = Med ("", secondsToDiffTime 0)


instance Neutral Intensity where
  neutral = None


instance Neutral (Entry Summaraized) where
  neutral = Entry [] m s p me dr c r
    where
      m  = ([[neutral]], [neutral])
      s  = ([neutral], neutral)
      p  = ([neutral], neutral)
      me = ([[neutral]],[neutral])
      dr = ([[neutral]],[neutral])
      c  = ([[neutral]],[neutral])
      r  = ([neutral], neutral)


instance Combinable Cigarette where
  partialCombine = unsafeCombineCigarettes
  combPrecon = sameCigarette


instance Combinable Drink where
  partialCombine = unsafeCombineDrinks
  combPrecon = sameDrink


instance Combinable Mood where
  partialCombine = unsafeCombineMoods
  combPrecon = sameMood


instance Monoid (Entry Summaraized) where
  mappend = (<>)
  mempty = neutral


instance Semigroup (Entry Summaraized) where
  (<>) = undefined


instance Summarizable Day where
  summary = id


instance Summarizable [Mood] where
  summary = condense


instance Summarizable Rating where
  summary = id


instance Summarizable Productivity where
  summary = id


instance Summarizable Sleep where
  summary = id


instance Summarizable [Cigarette] where
  summary = condense


instance Summarizable [Drink] where
  summary = condense


instance Summarizable [Meditation] where
  summary = id


instance Summarizable [Day] where
  summary = id


instance Summarizable [Rating] where
  summary xss = (xss, toEnum . (`div` length xss) . sum $ fmap fromEnum xss)


instance Summarizable [Productivity] where
  summary = label fold


instance Summarizable [Sleep] where
  summary = label (uncurry SP . bimap averageDiffTime averageDiffTime . unzip . map (\(SP x y) -> (x,y)))
    where
      averageDiffTime times
        = secondsToDiffTime .  (`div` (toInteger $ length times)) . sum $ map diffTimeToSeconds times


instance Summarizable [[Mood]] where
  summary = label (concatMap condense)


instance Summarizable [[Drink]] where
  summary = label (concatMap condense)


instance Summarizable [[Cigarette]] where
  summary = label (concatMap condense)


instance Summarizable [[Meditation]] where
  summary = label fold

instance Summarizable (Entry Parsed) where
  summary (Entry d m s p me dr c r) =
     Entry (summary [d]) (summary [m]) (summary [s])
           (summary [p]) (summary [me]) (summary [dr])
           (summary [c]) (summary [r])

instance Summarizable [Entry Summaraized] where
  summary = fold
