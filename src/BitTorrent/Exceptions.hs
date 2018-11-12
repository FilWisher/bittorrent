module BitTorrent.Exceptions
    ( ParseMessageException(..)
    ) where

import Control.Exception

data ParseMessageException = ParseMessageException String
    deriving (Show)
instance Exception ParseMessageException
