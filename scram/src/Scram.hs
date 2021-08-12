{-# Language OverloadedStrings #-}
{-# Language RecordWildCards #-}
{-# Language ImportQualifiedPost #-}
{-# Language BlockArguments #-}
{-# Language LambdaCase #-}
{-# Language ViewPatterns #-}
module Scram (
  -- * Transaction state types
  Scram1,
  Scram2,
  -- * Transaction step functions
  initiateScram,
  addServerFirst,
  addServerFinal,
  ) where

import Control.Monad ( guard, unless )
import Data.Bits ( xor )
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.ByteString.Base64 qualified as B64
import Data.ByteString.Char8 qualified as B8
import Data.List ( foldl1' )
import GHC.Word ( Word8 )
import OpenSSL.EVP.Digest ( Digest, digestBS, getDigestByName, hmacBS )

-- | SCRAM state waiting for server-first-message
data Scram1 = Scram1
  { scram1Digest          :: Digest     -- ^ underlying cryptographic hash function
  , scram1Password        :: ByteString -- ^ password
  , scram1CbindInput      :: ByteString -- ^ cbind-input
  , scram1Nonce           :: ByteString -- ^ c-nonce
  , scram1ClientFirstBare :: ByteString -- ^ client-first-bare
  }

-- | Construct client-first-message and extra parameters
-- needed for 'addServerFirst'.
initiateScram ::
  Digest ->
  ByteString {- ^ authentication ID -} ->
  ByteString {- ^ authorization ID  -} ->
  ByteString {- ^ password          -} ->
  ByteString {- ^ nonce             -} ->
  (ByteString, Scram1)
initiateScram digest user authzid pass nonce =
  (clientFirstMessage, Scram1
    { scram1Digest = digest
    , scram1Password = pass
    , scram1CbindInput = B64.encode gs2Header
    , scram1Nonce = nonce
    , scram1ClientFirstBare = clientFirstMessageBare
    })
  where
    clientFirstMessage = gs2Header <> clientFirstMessageBare
    gs2Header = "n," <> encodeUsername authzid <> ","
    clientFirstMessageBare = "n=" <> encodeUsername user <> ",r=" <> nonce

-- | SCRAM state waiting for server-final-message
newtype Scram2 = Scram2
  { scram2ServerSignature :: ByteString -- ^ base64 encoded expected value
  }

-- | Add server-first-message to current SCRAM transaction,
-- compute client-final-message and next state for 'addServerFinal'.
addServerFirst ::
  Scram1     {- ^ output of 'initiateScram' -} ->
  ByteString {- ^ server first message -} ->
  Maybe (ByteString, Scram2)
addServerFirst Scram1{..} serverFirstMessage =

  do -- Parse server-first-message
     ("r", nonce) :
       ("s", B64.decode -> Right salt) :
       ("i", B8.readInt -> Just (iterations, "")) :
       _extensions
       <- Just (parseMessage serverFirstMessage)

     -- validate nonce given by server includes ours and isn't empty
     guard (B.isPrefixOf scram1Nonce nonce && scram1Nonce /= nonce)

     let clientFinalWithoutProof = "c=" <> scram1CbindInput <> ",r=" <> nonce

     let authMessage =
           scram1ClientFirstBare <> "," <>
           serverFirstMessage <> "," <>
           clientFinalWithoutProof

     let (clientProof, serverSignature) =
           crypto scram1Digest scram1Password salt iterations authMessage

     let proof = "p=" <> B64.encode clientProof
     let clientFinalMessage = clientFinalWithoutProof <> "," <> proof

     let scram2 = Scram2 { scram2ServerSignature = B64.encode serverSignature }
     Just (clientFinalMessage, scram2)

-- | Add server-final-message to transaction and compute validatity of
-- the whole transaction.
addServerFinal ::
  Scram2     {- ^ output of 'addServerFirst' -} ->
  ByteString {- ^ server-final-message   -} ->
  Bool       {- ^ transaction succeeded? -}
addServerFinal Scram2{..} serverFinalMessage =
  case parseMessage serverFinalMessage of
    ("v", sig) : _extensions -> sig == scram2ServerSignature
    _ -> False

-- | Big endian encoding of a 32-bit number 1.
int1 :: ByteString
int1 = B.pack [0,0,0,1]

xorBS :: ByteString -> ByteString -> ByteString
xorBS x y = B.pack (B.zipWith xor x y)

-- | Iterated, password-based, key-derivation function.
hi ::
  Digest     {- ^ underlying cryptographic hash function -} ->
  ByteString {- ^ secret -} ->
  ByteString {- ^ salt -} ->
  Int        {- ^ iterations -} ->
  ByteString {- ^ salted, iterated hash of secret -}
hi digest str salt n = foldl1' xorBS (take n us)
  where
    u1 = hmacBS digest str (salt <> int1)
    us = iterate (hmacBS digest str) u1

-- | Break up a SCRAM message into its underlying key-value association list.
parseMessage :: ByteString -> [(ByteString, ByteString)]
parseMessage msg =
  [case B8.break ('='==) entry of
     (key, value) -> (key, B.drop 1 value)
  | entry <- B8.split ',' msg]

-- | Tranform all the SCRAM parameters into a @ClientProof@
-- and @ServerSignature@.
crypto ::
  Digest      {- ^ digest       -} ->
  ByteString  {- ^ password     -} ->
  ByteString  {- ^ salt         -} ->
  Int         {- ^ iterations   -} ->
  ByteString  {- ^ auth message -} ->
  (ByteString, ByteString) {- ^ client-proof, server-signature -}
crypto digest password salt iterations authMessage =
  (clientProof, serverSignature)
  where
    saltedPassword  = hi       digest password salt iterations
    clientKey       = hmacBS   digest saltedPassword "Client Key"
    storedKey       = digestBS digest clientKey
    clientSignature = hmacBS   digest storedKey authMessage
    clientProof     = xorBS clientKey clientSignature
    serverKey       = hmacBS   digest saltedPassword "Server Key"
    serverSignature = hmacBS   digest serverKey authMessage

-- | Encode usersnames so they fit in the comma/equals delimited
-- SCRAM message format.
encodeUsername :: ByteString -> ByteString
encodeUsername = B8.concatMap \case
    ',' -> "=2C"
    '=' -> "=3D"
    x   -> B8.singleton x
