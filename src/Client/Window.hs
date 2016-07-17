{-# Language TemplateHaskell #-}

module Client.Window where

import Control.Lens
import Client.Message
import Graphics.Vty.Image (Image)
import Data.Text (Text)

data WindowLine = WindowLine
  { _wlBody  :: MessageBody
  , _wlText  :: Text
  , _wlImage :: Image
  }

data Window = Window
  { _winMessages :: ![WindowLine]
  , _winUnread   :: !Int
  , _winMention  :: !Bool
  }

makeLenses ''Window
makeLenses ''WindowLine

emptyWindow :: Window
emptyWindow = Window
  { _winMessages = []
  , _winUnread   = 0
  , _winMention  = False
  }

addToWindow :: WindowLine -> Window -> Window
addToWindow msg = over winMessages (cons msg)
                . over winUnread   (+1)

windowSeen :: Window -> Window
windowSeen = set winUnread 0
           . set winMention False
