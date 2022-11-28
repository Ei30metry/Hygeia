module Config where

import           Control.Lens ( makeLenses )

import           GHC.Generics

data InfoConf = Info { _name  :: String
                     , _email :: String } deriving (Eq, Show)


newtype DaemonConfig = DConf { _runDaemon :: Bool } deriving (Show, Eq)


data OptHeader = OptH { _meditation :: Bool
                      , _alcohol    :: Bool
                      , _cigarette  :: Bool } deriving (Eq, Show)

newtype GenTemplate = GenTemp { _generateTemplate :: Bool } deriving (Eq, Show)


data TemplateConf = TempConf { _genTemplate     :: GenTemplate
                             , _optionalHeaders :: OptHeader } deriving (Eq, Show)


data ReportConf = RepConf { _emailReport          :: Bool
                          , _emailReportFrequency :: Int } deriving (Eq, Show)

data Config = Config { _info     :: InfoConf
                     , _daemon   :: DaemonConfig
                     , _template :: TemplateConf
                     , _report   :: ReportConf } deriving (Show, Eq, Generic)

makeLenses ''Config
