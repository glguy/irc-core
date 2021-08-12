{-# Language OverloadedStrings #-}
{-# Language RecordWildCards #-}
{-# Language ImportQualifiedPost #-}
module Scram (
  Scram1,
  Scram2,
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

data Scram1 = Scram1
  { scram1Digest :: Digest
  , scram1Password :: ByteString
  , scram1CbindInput :: ByteString
  , scram1Nonce :: ByteString
  , scram1ClientFirstBare :: ByteString
  }

initiateScram ::
  Digest ->
  ByteString {- ^ user -} ->
  ByteString {- ^ authzid -} ->
  ByteString {- ^ password -} ->
  ByteString {- ^ nonce -} ->
  ByteString {- ^ channel binding -} ->
  (ByteString, Scram1)
initiateScram digest user authzid pass nonce binding =
  (msg, Scram1
    { scram1Digest = digest
    , scram1Password = pass
    , scram1CbindInput = B64.encode (gs2Header <> binding)
    , scram1Nonce = nonce
    , scram1ClientFirstBare = bare
    })
  where
    msg = gs2Header <> bare
    gs2Header = "n," <> authzid <> ","
    bare = clientFirstMessageBare user nonce

data Scram2 = Scram2
  { scram2ServerSignature :: ByteString
  }

addServerFirst ::
  Scram1 ->
  ByteString {- ^ server first message -} ->
  Maybe (ByteString, Scram2)
addServerFirst Scram1{..} serverFirstMessage =

  do let serverFields = parseMessage serverFirstMessage
     nonce            <-                lookup "r" serverFields
     Right salt       <- B64.decode <$> lookup "s" serverFields
     (iterations, "") <- B8.readInt =<< lookup "i" serverFields

     guard (B.isPrefixOf scram1Nonce nonce && scram1Nonce /= nonce)

     let clientFinalWithoutProof = "c=" <> scram1CbindInput <> ",r=" <> nonce
     let authMessage = scram1ClientFirstBare <> "," <>
                       serverFirstMessage <> "," <>
                       clientFinalWithoutProof
     let (clientProof, serverSignature) =
           crypto scram1Digest scram1Password salt iterations authMessage
     let proof = "p=" <> B64.encode clientProof
     let clientFinalMessage = clientFinalWithoutProof <> "," <> proof
     Just (clientFinalMessage, Scram2 {
       scram2ServerSignature = B64.encode serverSignature
     })

addServerFinal ::
  Scram2 ->
  ByteString {- ^ server-final-message -} ->
  Bool
addServerFinal Scram2{..} serverFinalMessage =
  case lookup "v" (parseMessage serverFinalMessage) of
    Just v -> v == scram2ServerSignature
    _ -> False

int1 :: ByteString
int1 = B.pack [0,0,0,1]

packZipWith :: (Word8 -> Word8 -> Word8) -> ByteString -> ByteString -> ByteString
packZipWith f x y = B.pack (B.zipWith f x y)

hi :: Digest -> ByteString -> ByteString -> Int -> ByteString
hi digest str salt n = foldl1' (packZipWith xor) (take n us)
  where
    u1 = hmacBS digest str (salt <> int1)
    us = iterate (hmacBS digest str) u1

saslPrep :: ByteString -> ByteString
saslPrep x = x

clientFirstMessageBare ::
  ByteString {- ^ username -} ->
  ByteString {- ^ nonce -} ->
  ByteString
clientFirstMessageBare user nonce =
  "n=" <> saslPrep user <> ",r=" <> nonce

parseMessage :: ByteString -> [(ByteString, ByteString)]
parseMessage msg =
  [case B8.break ('='==) entry of
     (key, value) -> (key, B.drop 1 value)
  | entry <- B8.split ',' msg]

crypto ::
  Digest ->
  ByteString {- ^ password -} ->
  ByteString {- ^ salt -} ->
  Int {- ^ iterations -} ->
  ByteString {- auth message -} ->
  (ByteString, ByteString)
crypto digest password salt iterations authMessage =
  (clientProof, serverSignature)
  where
    saltedPassword = hi digest password salt iterations
    storedKey = digestBS digest clientKey
    clientSignature = hmacBS digest storedKey authMessage
    clientKey = hmacBS digest saltedPassword "Client Key"
    serverKey = hmacBS digest saltedPassword "Server Key"
    serverSignature = hmacBS digest serverKey authMessage
    clientProof = packZipWith xor clientKey clientSignature


manual answer =
  do Just sha2 <- getDigestByName "SHA256"
     let (cmsg1, scram1) = initiateScram sha2 "glguy" "" "Z8FU55MpynSKhb7QRPi0n9OOaiRm93QZ" "rOprNGfwEbeRWgbNEkqO" ""
     print ("Client1:",cmsg1)

     let Right serverFirst = B64.decode answer
     let Just (cmsg2, scram2) = addServerFirst scram1 serverFirst
     print ("Client2:",cmsg2)
     print ("Sendback:")
     B8.putStrLn $ "AUTHENTICATE " <> B64.encode cmsg2

     print (scram2ServerSignature scram2)



demo =
  do Just sha2 <- getDigestByName "SHA256"
     let (cmsg1, scram1) = initiateScram sha2 "user" "" "pencil" "rOprNGfwEbeRWgbNEkqO" ""
     print ("Client1:",cmsg1)
     unless (cmsg1 == "n,,n=user,r=rOprNGfwEbeRWgbNEkqO") (fail "Bad client first")

     let serverFirst = "r=rOprNGfwEbeRWgbNEkqO%hvYDpWUa2RaTCAfuxFIlj)hNlF$k0,s=W22ZaJ0SNY7soEsUEjb6gQ==,i=4096"
     let Just (cmsg2, scram2) = addServerFirst scram1 serverFirst
     print ("Client2:",cmsg2)
     unless (cmsg2 == "c=biws,r=rOprNGfwEbeRWgbNEkqO%hvYDpWUa2RaTCAfuxFIlj)hNlF$k0,p=dHzbZapWIk4jUhN+Ute9ytag9zjfMHgsqmmiz7AndVQ=")
       (fail "bad client 2")

     let serverFinalMessage = "v=6rriTRBi23WpRR/wtup+mMhUZUn/dB5nLTJRsjl95G4="
     let happy = addServerFinal scram2 serverFinalMessage
     unless happy
       (fail "bad server verify")
