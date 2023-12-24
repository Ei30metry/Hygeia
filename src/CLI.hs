module CLI where

import qualified Data.ByteString     as B

import           Options.Applicative


-- For now
data Params = Params

-- Environment for our computation
data Env = Env { configFile       :: FilePath
               , generateTemplate :: Bool
               , startDaemon      :: Bool
               , entryDirectory   :: FilePath }
               deriving (Show, Eq)

parseCmd :: Parser Params
parseCmd = pure Params

cliGenTemplate = undefined

cliCompEntries = undefined

cliPrintHelp = undefined
