-- |

module Computation.Monad where

import           Computation.Types

import qualified Config                     as C

import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.Writer

import           Daemon

import           Data.ByteString.Lazy.Char8 ( ByteString )
import           Data.Coerce
import           Data.Foldable
import           Data.Kind
import           Data.Time                  ( Day )
import           Data.Vector                ( Vector, fromList, toList )
import qualified Data.Vector                as V

import           System.Info

data CompError

-- Some of the things that we need to compute are not detailed in configuration file

type Days = Int

data ConfCommand = Set ConfigField FieldValue
                 | Edit
                 | Cat ConfigField -- for now
                 deriving (Show, Eq)

{-
Example:

template takes a Bool
entry-directory and name, take a String
-}
data FieldValue = BVal Bool
                | SVal String
                deriving (Show, Eq)


data ConfigField = UserInfoField
                 | EntryDirectoryField
                 | DaemonField
                 | TemplateField
                 | OptionalHeaderField OHeaderField
                 deriving (Show, Eq)


data OHeaderField = OMeditation
                  | OAlcohol
                  | OCigarette
                  deriving (Show, Eq)



data EntryField = MoodField
                | MeditationField
                | CigaretteField
                | DrinkField
                | RatingField
                | SleepField
                | ProductivityField
                deriving (Show, Eq)

-- rename Env to something else
data Action = Summary [EntryField] Interval
            | Config ConfCommand
            | Generete Interval
            | Daemon DaemonCommand
            deriving Show


data Interval = Date Day
              | Month Int
              | Day Int
              | Year Int
              | Week Int
              | All
              deriving (Show, Eq)

data DaemonCommand = Start | Restart | Shutdown | Stop
  deriving (Eq, Show)

defaultConfig :: C.Config
defaultConfig = C.Config userInfo' daemonConf' templateConf' optHeader' "/Users/artin/Documents/Hygeia/"
  where
    osInfo' = C.OsInfo os (serviceManager os)
    daemonConf' = C.DaemonConf True osInfo'
    optHeader' = C.OptH True True True
    templateConf' = C.TempConf True
    userInfo' = C.Info "Unknown"

-- type Comp a = ReaderT Env (Except CompError) a
type Comp a = ReaderT C.Config (Except CompError) a

runComp :: Comp a -> C.Config -> Either CompError a
runComp comp = runExcept . runReaderT comp

-- saveEntry :: Entry -> IO ()
saveEntry = undefined

combineEntries = undefined

initialEntry = undefined

-- makeSenseOfEntries :: Vector Entry -> Entry
-- makeSenseOfEntries = foldr combineEntries initialEntry

withComp = withReaderT

mapComp = mapReaderT


-- not good enough, make it more strict. it shouldn't work for any type that is the result of a type constructor to a type. Not possible!
-- The only way to ensure the above invariant is to write an inductive instance declaration. Which only works for the things we have written.
type family UnListLike a where
  UnListLike (Vector a) = a
  UnListLike ([a])      = a
  UnListLike a          = a

-- Remember that we don't have a way to combine incompatible moods. Ex: Happy High, Sad Low
-- therefore, we need to keep them both, hence Vector Mood, (Moods)
-- same thing for ()
-- For things like Moods, Meditaiotns, Cigarettes and Drinks, use a special type 'BlahSummary', record more information
-- in order to pretty-print them down the road
type family SummaryContainer a | a -> a where
  SummaryContainer a = a


class Summarizable a where
  summary :: a -> SummaryContainer (UnListLike a)

instance {-# OVERLAPPABLE #-} ((SummaryContainer (UnListLike  a)) ~ a) => Summarizable a where
  summary = id

instance Summarizable (Vector a) => Summarizable [a] where
  summary = summary . fromList

instance Summarizable (Vector Rating) where
  summary xss = toEnum . (`div` length xss) . sum $ fmap fromEnum xss

instance Summarizable (Vector Productivity) where
  summary = fold

instance Summarizable (Vector Moods) where
  summary = undefined

instance Summarizable (Vector Sleep) where
  summary = averageSleepTime
    where
      averageSleepTime :: Vector Sleep -> Sleep
      averageSleepTime = undefined

instance Summarizable (Vector Drinks) where
  summary = undefined

instance Summarizable (Vector Cigarette) where
  summary = undefined

instance Summarizable (Vector Meditations) where
  summary = fold

instance Summarizable Entry where
  summary (Entry d m s p me dr c r) = Entry d (summary m) (summary s) (summary p) (summary me) (summary dr) (summary c) (summary r)
