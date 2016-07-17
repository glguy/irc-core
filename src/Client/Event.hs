module Client.Event where

import Data.ByteString (ByteString)
import Data.Time (ZonedTime)
import Control.Exception
import Graphics.Vty
import Client.Message

data ClientEvent
  = NetworkLine !NetworkName !ZonedTime !ByteString
  | NetworkError !NetworkName !ZonedTime !SomeException
  | NetworkClose !NetworkName !ZonedTime
  | VtyEvent !Event
  | TimerEvent
