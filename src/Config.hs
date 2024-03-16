{-# LANGUAGE TemplateHaskell #-}

module Config where

import           Control.Lens                     ( makeLenses )
import           Control.Lens.Operators
import           Control.Monad

import           Daemon

import           Data.ByteString.Char8            ( ByteString )
import           Data.Map                         ( Map )
import qualified Data.Map                         as M
import           Data.Text                        ( Text, unpack )
import           Data.Time                        ( Day )
import           Data.Time.Calendar
import           Data.Time.Calendar.Month
import           Data.Time.Calendar.MonthDay
import           Data.Time.Calendar.WeekDate
-- import           Data.Time.Calendar.Week
import           Data.YAML

import           GHC.Generics

import           System.Info
import           System.Posix.ByteString.FilePath


newtype UserInfo = Info { _name  :: String } deriving (Eq, Show, Generic)

data DaemonConf = DaemonConf { _runDaemon :: Bool
                             , _osInfo    :: OsInfo } deriving (Show, Eq, Generic)

type OS = String

data OsInfo = OsInfo { _osName             :: OS
                     , _serviceManagerInfo :: ServiceManager } deriving (Show, Eq, Generic)



data OptHeader = OptH { _meditation :: Bool
                      , _alcohol    :: Bool
                      , _cigarette  :: Bool } deriving (Eq, Show, Generic)

newtype TemplateConf = TempConf { _genTemplate :: Bool } deriving (Eq, Show, Generic)

data Config = Config { _userInfo        :: UserInfo
                     , _daemonConf      :: DaemonConf
                     , _templateConf    :: TemplateConf
                     , _optionalHeaders :: OptHeader
                     , _entryDirectory  :: FilePath } deriving (Show, Eq, Generic)


-- For now nothing is actually nested, but I have a feeling that things will escalate quickly.
makeLenses ''UserInfo
makeLenses ''DaemonConf
makeLenses ''OptHeader
makeLenses ''Config
makeLenses ''TemplateConf
makeLenses ''OsInfo


instance FromYAML Config where
    parseYAML = withMap "Config" $ \m -> Config
      <$> (m .: "name" >>= withStr "name" (return . Info . unpack))
      <*> (m .: "daemon" >>= withBool "daemon" (return . dconf))
      <*> (m .: "template" >>= withBool "template" (return . TempConf))
      <*> m .: "optional-headers"
      <*> (unpack <$> (m .: "entry-directory" >>= withStr "entry-directory" return))
     where
       dconf x = DaemonConf x osInfo'
       osInfo' = OsInfo os (serviceManager os)


instance FromYAML OptHeader where
  parseYAML = withMap "OptHeader" $ \m -> do
    med <- m .: "meditation" >>= withBool "meditation" return
    alco <- m .: "alcohol" >>= withBool "alcohol" return
    cig <- m .: "cigarette" >>= withBool "cigarette" return
    return (OptH med alco cig)


data ConfCommand = Set ConfigField FieldValue
                 | Edit
                 | Cat ConfigField -- for now
                 deriving (Show, Eq)


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
                  | ODrink
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


type Days = Int


data Interval = Date Day
              | Months Int
              | Days Int
              | Years Int
              | Weeks Int
              | DefInterval DefaultInterval
              deriving (Show, Eq)

-- | Takes the interval and the first day that the user wrote an entry current day as its arguments
buildDays :: Interval -> Day -> Day -> Maybe [Day]
buildDays interval firstDay today
  | Just firstDay > (findTargetDay interval firstDay today)
    = findTargetDay interval firstDay today >>= \tday -> Just [tday .. today]
  | otherwise = Nothing


validDate :: Day -> Bool
validDate = undefined


-- NOTE we need to take the first day in this function, because the user might request the "All" interval
findTargetDay :: Interval -> Day -> Day -> Maybe Day
findTargetDay interval firstDay day
  = case interval of
      Date x            -> Just x
      Months x          -> Just $ undefined
      Days x            -> Just $ addDays (- fromIntegral x) day
      Years x           -> Just $ undefined
      Weeks x           -> Just $ undefined
      DefInterval Today -> Just day
      DefInterval All   -> Just firstDay



monthDays :: Bool -> Map MonthOfYear Int
monthDays leapYear
  = M.fromList [(January,31), february leapYear, (March,31), (April,30)
               ,(May,31), (June,30), (July,31), (August,31), (September,30)
               ,(October,31), (November,30), (December,31)]
  where
    february True  = (February, 29)
    february False = (February, 28)


data DefaultInterval = All | Today deriving (Eq, Show)


data DaemonCommand = Start | Restart | Shutdown | Stop
  deriving (Eq, Show)


defaultConfig :: Config
defaultConfig = Config userInfo' daemonConf' templateConf' optHeader' "/Users/artin/Documents/Hygeia/"
  where
    osInfo'       = OsInfo os (serviceManager os)
    daemonConf'   = DaemonConf True osInfo'
    optHeader'    = OptH True True True
    templateConf' = TempConf True
    userInfo'     = Info "Unknown"
