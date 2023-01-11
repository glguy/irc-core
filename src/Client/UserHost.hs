{-# Language TemplateHaskell #-}
{-|
Module      : Client.UserHost
Description : Type for tracking user, host and account
Copyright   : (c) Eric Mertens, 2019
License     : ISC
Maintainer  : emertens@gmail.com

-}

module Client.UserHost
  (
  -- * User information
    UserAndHost(..)
  , uhUser
  , uhHost
  , uhAccount
  ) where

import Control.Lens (makeLenses)
import Data.Text (Text)

-- | Pair of username and hostname. Empty strings represent missing information.
data UserAndHost = UserAndHost
  { _uhUser    :: {-# UNPACK #-}!Text -- ^ username
  , _uhHost    :: {-# UNPACK #-}!Text -- ^ hostname
  , _uhAccount :: {-# UNPACK #-}!Text -- ^ services account
  }
  deriving (Read, Show)

makeLenses ''UserAndHost
