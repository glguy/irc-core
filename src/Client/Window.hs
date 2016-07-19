{-# Language TemplateHaskell #-}

module Client.Window where

import Control.Lens
import Client.Message
import Graphics.Vty.Image (Image)
import Data.Text (Text)

data WindowLine = WindowLine
  { _wlBody      :: !MessageBody
  , _wlText      :: !Text
  , _wlImage     :: !Image
  , _wlFullImage :: !Image
  }

data Window = Window
  { _winMessages :: ![WindowLine]
  , _winUnread   :: !Int
  , _winMention  :: !Bool
  }

data WindowLineImportance
  = WLBoring
  | WLNormal
  | WLImportant
  deriving (Eq, Show)

makeLenses ''Window
makeLenses ''WindowLine

emptyWindow :: Window
emptyWindow = Window
  { _winMessages = []
  , _winUnread   = 0
  , _winMention  = False
  }

addToWindow :: WindowLineImportance -> WindowLine -> Window -> Window
addToWindow importance msg win = Window
    { _winMessages = msg : _winMessages win
    , _winUnread   = _winUnread win + (if importance == WLBoring then 0 else 1)
    , _winMention  = _winMention win || importance == WLImportant
    }

windowSeen :: Window -> Window
windowSeen = set winUnread 0
           . set winMention False
