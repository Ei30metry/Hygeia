{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections   #-}

-- | Types used to compute summaries

module Computation.Types where

import           Computation.Error
import           Computation.Utils

import           Control.Monad      ( foldM, join, (<=<), (=<<) )

import           Data.Bifunctor
import           Data.Either
import           Data.Foldable
import           Data.List
import           Data.Ratio
import           Data.Time          ( Day, DiffTime, defaultTimeLocale,
                                      formatTime, secondsToDiffTime )

import           Prettyprinter
import           Prettyprinter.Util

import           Text.Read          ( readEither )


data Mood = Angry Intensity
          | Sad Intensity
          | Neutral
          | Happy Intensity
          | Excited Intensity
          deriving (Read, Eq, Ord, Show)

-- NOTE This is here, because we don't want thing like ""
moodMeaning :: Mood -> Mood
moodMeaning (Angry None)   = Neutral
moodMeaning (Sad None)     = Neutral
moodMeaning (Happy None)   = Neutral
moodMeaning (Excited None) = Neutral
moodMeaning Neutral        = Neutral
moodMeaning x              = x


instance Pretty Mood where
  pretty (Angry a)   = "Angry ::" <+> pretty a
  pretty (Sad a)     = "Sad ::" <+> pretty a
  pretty (Neutral)   = "Neutral"
  pretty (Happy a)   = "Happy ::" <+> pretty a
  pretty (Excited a) = "Excited ::" <+> pretty a


instance {-# OVERLAPS #-} Pretty [Mood]


instance {-# OVERLAPS #-} Pretty [[Mood]]


instance Neutral Mood where
  neutral = Neutral


instance Combinable Mood where
  combPrecon (Angry _) (Angry _)     = True
  combPrecon (Sad _) (Sad _)         = True
  combPrecon Neutral Neutral         = True
  combPrecon (Happy _) (Happy _)     = True
  combPrecon (Excited _) (Excited _) = True
  combPrecon _ _                     = False

  partialCombine (Angry x) (Angry y)     = Angry (x <> y)
  partialCombine (Sad x) (Sad y)         = Sad (x <> y)
  partialCombine Neutral Neutral         = Neutral
  partialCombine (Happy x) (Happy y)     = Happy (x <> y)
  partialCombine (Excited x) (Excited y) = Excited (x <> y)
  partialCombine Neutral x               = x
  partialCombine x Neutral               = x
  partialCombine _ _                     = undefined


instance Summarizable [Mood] where
  summary = condense . map moodMeaning


instance Summarizable [[Mood]] where
  summary = label (concatMap condense)


instance Pretty Day


instance {-# OVERLAPS #-} Pretty [Day]

-- | Intensity of a mood
data Intensity = None      -- Only here because of neutral
               | Low
               | Medium
               | High
               | Extreme
               deriving (Show, Read, Eq, Ord, Enum, Bounded)


instance Pretty Intensity


instance Neutral Intensity where
  neutral = None


instance Semigroup Intensity where
  x <> y
    | fromEnum x == fromEnum y = x
    | fromEnum x > fromEnum y = x
    | otherwise = y


instance Monoid Intensity where
  mappend = (<>)
  mempty = neutral

-- | Rating the day
data Rating = Awful
            | Bad
            | Normal
            | Good
            | Great
            deriving (Show, Eq, Ord, Enum, Read, Bounded)


instance Pretty Rating where
  pretty x = "Rating: " <+> viaShow x


instance Neutral Rating where
  neutral = Normal


instance Semigroup Rating where
  x <> y
    | fromEnum x == fromEnum y = x
    | fromEnum x > fromEnum y = x
    | otherwise = y


instance Monoid Rating where
  mempty = neutral
  mappend = (<>)


instance Summarizable Rating where
  summary = id


instance Summarizable [Rating] where
  summary = label mconcat


type Name = String


data Drink = Drink { drinkName :: String
                   , shots     :: Int }
           deriving (Eq, Ord, Show)


instance Pretty Drink where
  pretty (Drink d s)
    = vsep ["Drink: "
           ,indent 7 ("Name  =" <+> viaShow d)
           ,indent 7 ("Shots =" <+> viaShow s)]
      <> line


instance {-# OVERLAPS #-} Pretty [Drink] where
  pretty list
    = let
         namesAndShots = map ((\(x,y) -> indent 6 (vsep [("Name  =" <+> viaShow x)
                                                       ,("Shots =" <+> viaShow y)])
                                     <> line)
                             . \(Drink d s) -> (d,s)) list
      in
        "Drink:" <> line <> vsep namesAndShots


instance {-# OVERLAPS #-} Pretty [[Drink]]


instance Neutral Drink where
  neutral = Drink "" 0


instance Combinable Drink where
  partialCombine (Drink d s) (Drink _ s') = Drink d (s + s')
  combPrecon (Drink d _) (Drink d' _)    = d == d'


instance Summarizable [Drink] where
  summary = condense


instance Summarizable [[Drink]] where
  summary = label (concatMap condense)


instance Summarizable Day where
  summary = id


instance Summarizable [Day] where
  summary = id


data Sleep = SP { wakeUpTime :: DiffTime
                , sleepTime  :: DiffTime } deriving (Eq, Ord)


instance Semigroup Sleep where
  (SP w s) <> (SP w' s') = SP averageWakeUp averageSleep
    where
      averageWakeUp = secondsToDiffTime $ diffTimeToSeconds (w + w') `div` 2
      averageSleep  = secondsToDiffTime $ diffTimeToSeconds (s + s') `div` 2


instance Monoid Sleep where
  mempty = neutral
  mappend = (<>)


instance Neutral Sleep where
  neutral = SP (secondsToDiffTime 0) (secondsToDiffTime 0)


instance Show Sleep where
  show (SP w s) = mconcat ["Wake up =",formatTime defaultTimeLocale "%H:%M" w,"\n"
                          ,"Sleep   =",formatTime defaultTimeLocale "%H:%M" s]


instance Pretty Sleep where
  pretty (SP w s)
    = vsep ("Sleep:" : [indent 6 (pretty w), indent 6 (pretty s)])
    <> line


instance Summarizable Sleep where
  summary = id


instance Summarizable [Sleep] where
  summary = label mconcat


instance Pretty DiffTime where
  pretty = viaShow . formatTime defaultTimeLocale "%H:%M"


newtype Meditation = Med { unMed :: (String, DiffTime) } deriving (Eq, Ord)


instance Semigroup Meditation where
  Med (a,b) <> Med (c,d) = Med (a <> "," <> c, b + d)


instance Monoid Meditation where
  mempty = neutral
  mappend = (<>)


instance Neutral Meditation where
  neutral = Med ("", secondsToDiffTime 0)


instance Summarizable [Meditation] where
  summary = id


instance Summarizable [[Meditation]] where
  summary = label fold


instance Show Meditation where
  show (Med a) = show a


instance Pretty Meditation where
  pretty = undefined


instance {-# OVERLAPS #-} Pretty [Meditation] where
  pretty list
    = let
         total = sum $ map (snd . unMed) list
         meditations = map ((\x -> vsep [indent 15 (viaShow x)]) . fst . unMed) list
      in
        "Meditation:" <> line
        <> indent 2 ("Total time =" <+> pretty total) <> line
        <> indent 2 ("Sessions:") <> line
        <> vsep meditations <> line


mkMeditation :: String -> Either EntryError Meditation
mkMeditation minutes
  = let
       handle (Left y) = Left $ MeditationError ("Can't parse meditation: " ++ y)
       handle (Right x) = Right x
    in
       Med . (minutes,)
                    <$> (return . secondsToDiffTime . (60*)
                    =<< (handle . readEither @Integer) minutes)


newtype Productivity = Pro { unPro :: Rational } deriving (Eq, Ord)


instance Pretty Productivity where
  pretty x = "Productivity:" <+> viaShow x


instance Neutral Productivity where
  neutral = Pro 0


instance Show Productivity where
  show (Pro a) = mconcat [show (numerator a) ,"/",show (denominator a)]


instance Semigroup Productivity where
  (Pro a) <> (Pro b) = Pro $ a + b


instance Monoid Productivity where
  mappend = (<>)
  mempty  = Pro 0


instance Summarizable Productivity where
  summary = id


instance Summarizable [Productivity] where
  summary = label fold


data Cigarette = Cigarette { cigaretteName :: String
                           , numberSmoked  :: Double
                           , nicotine      :: Double
                           , tar           :: Double }
                           deriving (Eq, Ord, Show)


instance Pretty Cigarette where
  pretty (Cigarette c ns n t)
    = vsep ["Cigarette:",vsep (map (indent 10) ["Name     =" <+> viaShow c
                                               ,"Smoked   =" <+> viaShow ns
                                               ,"Nicotine =" <+> viaShow n
                                               ,"Tar      =" <+> viaShow t])] <> line


instance {-# OVERLAPS #-} Pretty [Cigarette] where
  pretty list
    = let
         parts = map ((\(c,ns,n,t) -> indent 10 (vsep ["Name     =" <+> viaShow c
                                                     ,"Smoked   =" <+> viaShow ns
                                                     ,"Nicotine =" <+> viaShow n
                                                     ,"Tar      =" <+> viaShow t]) <> line)
                     . \(Cigarette c ns n t) -> (c,ns,n,t)) list
      in
        "Cigarette:" <> line <> vsep parts


instance {-# OVERLAPS #-} Pretty [[Cigarette]]


instance Neutral Cigarette where
  neutral = Cigarette "" 0 0 0


instance Combinable Cigarette where
  partialCombine (Cigarette c ns n t) (Cigarette _ ns' _ _) = Cigarette c (ns + ns') n t
  combPrecon (Cigarette c _ _ _) (Cigarette c' _ _ _) = c == c'


instance Summarizable [Cigarette] where
  summary = condense


instance Summarizable [[Cigarette]] where
  summary = label (concatMap condense)


data Stage = Parsed | Summaraized deriving (Show, Eq)


data Entry a = Entry { entryDay          :: !(EntryType Day a)
                     , entryMoods        :: !(EntryType [Mood] a)
                     , entrySleep        :: !(EntryType Sleep a)
                     , entryProductivity :: !(EntryType Productivity a)
                     , entryMeditations  :: !(EntryType [Meditation] a)
                     , entryDrinks       :: !(EntryType [Drink] a)
                     , entryCigarettes   :: !(EntryType [Cigarette] a)
                     , entryRating       :: !(EntryType Rating a) }


type family EntryType t a where
  EntryType t Parsed = t
  EntryType t Summaraized = SummaryType [t]


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


instance Monoid (Entry Summaraized) where
  mappend = (<>)
  mempty = neutral


instance Semigroup (Entry Summaraized) where
  (Entry d m s p me dr c r) <> (Entry d' m' s' p' me' dr' c' r')
    = Entry undefined undefined undefined undefined undefined undefined undefined undefined


instance Summarizable (Entry Parsed) where
  summary (Entry d m s p me dr c r) =
     Entry (summary [d]) (summary [m]) (summary [s])
           (summary [p]) (summary [me]) (summary [dr])
           (summary [c]) (summary [r])


instance Summarizable [Entry Summaraized] where
  summary = fold


instance Pretty (Entry Summaraized) where
  pretty (Entry d m s p me dr c r)
    = indent 10 (pretty d)
    <> vsep [pretty s, pretty p
            ,pretty r, pretty m
            ,pretty me, pretty c
            ,pretty dr]
    <> line


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
