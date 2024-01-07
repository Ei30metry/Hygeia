-- | Daemon

module Daemon where

import           Config                ( DaemonConf (..) )

import           Data.ByteString.Char8 ( ByteString )

import           Opaleye

import           System.Directory
import           System.Process


runDaemon :: Int -> IO ()
runDaemon every = undefined

-- | Convert a daemon config to PLIST (needed by Launchd)
daemonConfToPlist :: DaemonConf -> ByteString
daemonConfToPlist = undefined
