{-# LANGUAGE OverloadedStrings #-}


module Configuration.Config(Config(..)) where

import           Data.Time


-- existential type to work on
data Freq where
  Year :: (n ~ Int) => n -> Freq
  Month :: (n ~ Int) => n -> Freq
  Week :: (n ~ Int) => n -> Freq
  Day :: (n ~ Int) => n -> Freq


-- This type is only used by the Config type
data OptHeader = Alcohol | Cigarette | Meditation deriving Eq

-- This Type is for the frequency of reports
instance Show Freq where
  show (Year n)  = "Every " ++ show n ++ "Year"
  show (Month n) = "Every " ++ show n ++ "Month"
  show (Week n)  = "Every " ++ show n ++ "Week"
  show (Day n)   = "Every " ++ show n ++ "Day"


-- The data constructors of this type will be generated automatically by the config file
data Config = Template { generate        :: Bool
                       , optionalHeaders :: [OptHeader]}

            | Report { emailReport          :: Bool
                     , email                :: String
                     , emailReportFrequency :: Freq }

            | Daemon { daemon :: Bool }
