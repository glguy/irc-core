{-# Language KindSignatures, GADTs #-}

module Client.Commands.Arguments.Spec where

import Control.Applicative
import Control.Applicative.Free
import Text.Read (readMaybe)

type Args r = Ap (Arg r)

data ArgumentShape = TokenArgument | RemainingArgument

data Arg :: * -> * -> * where
  Argument  :: ArgumentShape -> String -> (r -> String -> Maybe a) -> Arg r a
  Optional  :: Args r a -> Arg r (Maybe a)
  Extension :: String -> (r -> String -> Maybe (Args r a)) -> Arg r a

tokenArg :: String -> (r -> String -> Maybe a) -> Args r a
tokenArg name parser = liftAp (Argument TokenArgument name parser)

remainingArg :: String -> Args r String
remainingArg name = liftAp (Argument RemainingArgument name (\_ -> Just))

optionalArg :: Args r a -> Args r (Maybe a)
optionalArg = liftAp . Optional

extensionArg :: String -> (r -> String -> Maybe (Args r a)) -> Args r a
extensionArg name parser = liftAp (Extension name parser)

------------------------------------------------------------------------
-- Example

modeArgs :: Args r [String]
modeArgs = extensionArg "mode" $ \_ str ->
        if all (=='x') str then Just ((str:) <$> tokenList (length str))
                           else Nothing

simpleToken :: String -> Args r String
simpleToken name = tokenArg name (\_ -> Just)

messageArg :: Args r String
messageArg = remainingArg "message"

numberArg :: Args r Int
numberArg = tokenArg "number" (\_ -> readMaybe)

msgArgs :: Args r (String, String)
msgArgs = liftA2 (,) (simpleToken "target")
                     messageArg

joinArgs :: Args r (String, Maybe String)
joinArgs = liftA2 (,) (simpleToken "channels")
                      (optionalArg (simpleToken "keys"))

optionalTest :: Args r (String, Maybe Int, String)
optionalTest =
  liftA3 (,,) (simpleToken "first")
              (optionalArg numberArg)
              (simpleToken "second")

tokenList :: Int -> Args r [String]
tokenList 0 = pure []
tokenList n = liftA2 (:) (tokenArg "param" (\_ -> Just))
                         (tokenList (n-1))

------------------------------------------------------------------------
-- Docs

document :: Args r a -> String
document = unwords . runAp_ documentArg

documentArg :: Arg r a -> [String]
documentArg spec =
  case spec of
    Argument _ name _ -> [name]
    Optional subspec  -> ["[" ++ document subspec ++ "]"]
    Extension name _  -> [name]

------------------------------------------------------------------------
-- Render

{-
render :: r -> Args r a -> String -> String
render env spec str =
   let (img,rest) = runStateT (renderArgs env spec) str
   in img ++ rest

renderArgs :: r -> Args r a -> State String String
renderArgs env spec = runAp
  case spec of
    Argument TokenArgument _ _ ->
      do xs <- get
         let (lead, mid) = span (' '==) xs
             (tok , rest) = break (' '==) mid
         put rest
         return (lead ++ tok)
-}
