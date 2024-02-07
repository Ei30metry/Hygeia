{-# LANGUAGE TemplateHaskell #-}

module Config where

import           Control.Lens                     ( makeLenses )
import           Control.Lens.Operators

import           Daemon

import           Data.ByteString.Char8            ( ByteString )
import           Data.Text                        ( Text, unpack )
import           Data.Time                        ( Day )
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


type Days = Int


data Interval = Date Day
              | Month Int
              | Day Int
              | Year Int
              | Week Int
              | All
              deriving (Show, Eq)

data DaemonCommand = Start | Restart | Shutdown | Stop
  deriving (Eq, Show)
