{-# LANGUAGE DataKinds #-}
-- | Daemon related things

module Daemon where

import           Control.Monad

import           Data.ByteString.Char8 ( ByteString )
import           Data.Kind             ( Type )

import           Opaleye

import           System.Directory
import           System.Process


constructServiceManagerFile :: ServiceManager -> ByteString
constructServiceManagerFile sm = undefined

-- | Convert a daemon config to PLIST (needed by Launchd)

-- | Service manager of the major OS
data ServiceManager = Launchd
                    | Systemd -- will add support for other service managers later
                    | SrvMan deriving (Show, Eq)

macServiceManager = Launchd
linuxServiceManager = Systemd
windowsServiceManager = SrvMan


agentDir :: ServiceManager -> FilePath
agentDir Launchd = "Library/LaunchAgents/"
agentDir Systemd = undefined
agentDir SrvMan  = undefined


serviceManagerCommand :: ServiceManager -> ByteString
serviceManagerCommand Launchd = "launchctl"
serviceManagerCommand Systemd = "systemctl"
serviceManagerCommand SrvMan  = "SrvMan.exe"


data ServiceManagerFile = XML
                        -- | UnitFile
                        -- | windws stuff

pattern PLIST = XML
-- pattern UNIT = UnitFile format

serviceManagerFileType :: ServiceManager -> ServiceManagerFile
serviceManagerFileType = undefined
