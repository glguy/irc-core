{-# Language TemplateHaskell #-}
{-# Language DeriveGeneric #-}
module Client.State where

import           Client.ChannelState
import           Client.ConnectionState
import           Client.Event
import           Client.Message
import           Client.Window
import           Client.Configuration
import           Client.MessageRenderer
import           Control.Concurrent.Chan
import           Control.Lens
import           Data.HashMap.Strict (HashMap)
import           Data.Hashable
import           Data.List
import           Data.Maybe
import           Data.Map (Map)
import           Data.Time
import           GHC.Generics
import           Graphics.Vty
import           Irc.Identifier
import           Irc.Message
import           LensUtils
import           Network.Connection
import qualified Client.EditBox as Edit
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map as Map
import qualified Data.Text as Text

data ClientFocus
 = Unfocused
 | NetworkFocus NetworkName
 | ChannelFocus NetworkName Identifier
  deriving (Eq, Ord, Generic)

instance Hashable ClientFocus

data ClientState = ClientState
  { _clientWindows     :: !(Map ClientFocus Window)
  , _clientTextBox     :: !Edit.EditBox
  , _clientConnections :: !(HashMap NetworkName ConnectionState)
  , _clientWidth, _clientHeight :: !Int
  , _clientEvents :: !(Chan ClientEvent)
  , _clientVty :: !Vty
  , _clientFocus :: !ClientFocus
  , _clientConnectionContext :: !ConnectionContext
  , _clientConfig :: !Configuration
  }

makeLenses ''ClientState

clientInput :: ClientState -> String
clientInput = view (clientTextBox . Edit.content)

focusNetwork :: ClientFocus -> Maybe NetworkName
focusNetwork Unfocused = Nothing
focusNetwork (NetworkFocus network) = Just network
focusNetwork (ChannelFocus network _) = Just network

initialClientState ::
  Configuration ->
  ConnectionContext -> Vty -> Chan ClientEvent -> IO ClientState
initialClientState cfg cxt vty events =
  do (width,height) <- displayBounds (outputIface vty)
     return ClientState
        { _clientWindows           = _Empty # ()
        , _clientTextBox           = Edit.empty
        , _clientConnections       = HashMap.empty
        , _clientWidth             = width
        , _clientHeight            = height
        , _clientVty               = vty
        , _clientEvents            = events
        , _clientFocus             = Unfocused
        , _clientConnectionContext = cxt
        , _clientConfig            = cfg
        }

recordChannelMessage :: NetworkName -> Identifier -> ClientMessage -> ClientState -> ClientState
recordChannelMessage network channel msg st =
  over (clientWindows . at focus) (\w -> Just $! addToWindow wl (fromMaybe emptyWindow w)) st
  where
    focus = ChannelFocus network channel'
    wl = toWindowLine statusModes modes msg
    modes = computeMsgLineModes network channel' msg st

    -- on failure returns mempty/""
    possibleStatusModes = view (clientConnections . ix network . csStatusMsg) st
    (statusModes, channel') = splitStatusMsgModes possibleStatusModes channel

recordIrcMessage :: NetworkName -> MessageTarget -> ClientMessage -> ClientState -> ClientState
recordIrcMessage network target msg st =
  case target of
    TargetHidden -> st
    TargetNetwork -> recordNetworkMessage msg st
    TargetWindow chan -> recordChannelMessage network chan msg st
    TargetUser user -> foldl' (\st' chan -> overStrict
                                              (clientWindows . ix (ChannelFocus network chan))
                                              (addToWindow wl) st')
                              st chans
      where
        wl = toWindowLine "" "" msg
        chans =
          case preview (clientConnections . ix network . csChannels) st of
            Nothing -> []
            Just m  -> [chan | (chan, cs) <- HashMap.toList m, HashMap.member user (view chanUsers cs) ]

splitStatusMsgModes :: [Char] -> Identifier -> ([Char], Identifier)
splitStatusMsgModes possible ident = (Text.unpack modes, mkId ident')
  where
    (modes, ident') = Text.span (`elem` possible) (idText ident)

computeMsgLineModes :: NetworkName -> Identifier -> ClientMessage -> ClientState -> [Char]
computeMsgLineModes network channel msg st =
  case msgActor =<< preview (msgBody . _IrcBody) msg of
    Just user -> computeLineModes network channel user st
    Nothing   -> []

computeLineModes :: NetworkName -> Identifier -> Identifier -> ClientState -> [Char]
computeLineModes network channel user =
    view $ clientConnections . ix network
         . csChannels        . ix channel
         . chanUsers         . ix user

recordNetworkMessage :: ClientMessage -> ClientState -> ClientState
recordNetworkMessage msg st =
  over (clientWindows . at (NetworkFocus network))
       (\w -> Just $! addToWindow wl (fromMaybe emptyWindow w))
       st
  where
    network = view msgNetwork msg
    wl = toWindowLine "" "" msg

toWindowLine :: [Char] -> [Char] -> ClientMessage -> WindowLine
toWindowLine statusModes modes msg = WindowLine
  { _wlBody = view msgBody msg
  , _wlText = views msgBody msgText msg
  , _wlImage = msgImage localTime statusModes modes (view msgBody msg)
  }
  where
    localTime = views msgTime zonedTimeToLocalTime msg

clientTick :: ClientState -> IO ClientState
clientTick st =
     return $! over (clientWindows . ix (view clientFocus st)) windowSeen st

consumeInput :: ClientState -> ClientState
consumeInput = over clientTextBox Edit.success

advanceFocus :: ClientState -> ClientState
advanceFocus cs =
  case Map.split oldFocus windows of
    (l,r)
      | Just ((k,_),_) <- Map.minViewWithKey r -> success k
      | Just ((k,_),_) <- Map.minViewWithKey l -> success k
      | otherwise                              -> cs
  where
    success  = set clientFocus ?? cs
    oldFocus = view clientFocus cs
    windows  = view clientWindows cs

retreatFocus :: ClientState -> ClientState
retreatFocus cs =
  case Map.split oldFocus windows of
    (l,r)
      | Just ((k,_),_) <- Map.maxViewWithKey l -> success k
      | Just ((k,_),_) <- Map.maxViewWithKey r -> success k
      | otherwise                              -> cs
  where
    success  = set clientFocus ?? cs
    oldFocus = view clientFocus cs
    windows  = view clientWindows cs
