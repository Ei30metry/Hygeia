{-# LANGUAGE TemplateHaskell #-}

module Config where

import           Control.Lens                     ( makeLenses )
import           Control.Lens.Operators

import           Data.ByteString.Char8            ( ByteString )

import           GHC.Generics

import           System.Info
import           System.Posix.ByteString.FilePath


data UserInfo = Info { _name  :: ByteString
                     , _email :: ByteString } deriving (Eq, Show, Generic)


data DaemonConf = DaemonConf { _runDaemon :: Bool
                             , _osInfo    :: OsInfo } deriving (Show, Eq, Generic)

data LaunchService = LaunchService deriving (Eq, Show)

data OsInfo = OsInfo { _osName        :: String
                     , _launchService :: LaunchService } deriving (Show, Eq)

data EntryConf = EntryConf { _daemonConf     :: DaemonConf
                           , _templateConf   :: TemplateConf
                           , _entryDirectory :: FilePath }deriving (Eq, Show)

data OptHeader = OptH { _meditation :: Bool
                      , _alcohol    :: Bool
                      , _cigarette  :: Bool } deriving (Eq, Show)

data TemplateConf = TempConf { _genTemplate     :: Bool
                             , _optionalHeaders :: OptHeader } deriving (Eq, Show, Generic)


data ReportConf = RepConf { _emailReport          :: Bool
                          , _emailReportFrequency :: Int } deriving (Eq, Show, Generic)

data Config = Config { _userInfo  :: UserInfo
                     , _entryConf :: EntryConf
                     , _report    :: ReportConf } deriving (Show, Eq, Generic)


makeLenses ''UserInfo
makeLenses ''DaemonConf
makeLenses ''EntryConf
makeLenses ''OptHeader
makeLenses ''TemplateConf
makeLenses ''ReportConf
makeLenses ''Config
