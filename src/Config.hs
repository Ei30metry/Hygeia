{-# LANGUAGE TemplateHaskell #-}

module Config where

import           Control.Lens                     ( makeLenses )
import           Control.Lens.Operators

import           Daemon

import           Data.ByteString.Char8            ( ByteString )
import           Data.YAML                        ( FromYAML, ToYAML )

import           GHC.Generics

import           System.Info
import           System.Posix.ByteString.FilePath


data UserInfo = Info { _name  :: ByteString
                     , _email :: Maybe ByteString } deriving (Eq, Show, Generic)

data DaemonConf = DaemonConf { _runDaemon :: Bool
                             , _osInfo    :: OsInfo } deriving (Show, Eq, Generic)

type OS = String

data OsInfo = OsInfo { _osName             :: OS
                     , _serviceManagerInfo :: ServiceManager } deriving (Show, Eq)



data EntryConf = EntryConf { _daemonConf     :: DaemonConf
                           , _templateConf   :: TemplateConf
                           , _entryDirectory :: FilePath }deriving (Eq, Show)

data OptHeader = OptH { _meditation :: Bool
                      , _alcohol    :: Bool
                      , _cigarette  :: Bool } deriving (Eq, Show)

data TemplateConf = TempConf { _genTemplate     :: Bool
                             , _optionalHeaders :: OptHeader } deriving (Eq, Show, Generic)

data Config = Config { _userInfo  :: UserInfo
                     , _entryConf :: EntryConf } deriving (Show, Eq, Generic)

defaultConfig :: Config
defaultConfig = Config userInfo' entryConf'
  where
    osInfo' = OsInfo os Launchd
    daemonConf' = DaemonConf True osInfo'
    optHeader' = OptH True True True
    templateConf' = TempConf True optHeader'
    userInfo' = Info "Artin Ghasivand" (Just "ghasivand.artin@gmail.com")
    entryConf' = EntryConf daemonConf' templateConf' "/Users/artin/Documents/Hygeia/"


makeLenses ''UserInfo
makeLenses ''DaemonConf
makeLenses ''EntryConf
makeLenses ''OptHeader
makeLenses ''TemplateConf
makeLenses ''Config
makeLenses ''OsInfo
