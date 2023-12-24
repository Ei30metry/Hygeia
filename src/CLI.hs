module CLI where

import qualified Data.ByteString     as B

import           Options.Applicative


-- for now
data Params = Params


parseCmd :: Parser Params
parseCmd = pure Params


cliGenTemplate = undefined

cliCompEntries = undefined

cliPrintHelp = undefined
