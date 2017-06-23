{-# Language OverloadedStrings, GADTs #-}

module Client.Commands.Arguments.Renderer (render) where

import Control.Lens
import Client.Image.PackedImage
import Client.Image.Palette
import Client.Commands.Arguments.Spec
import Control.Monad.Trans.State
import Control.Applicative.Free
import Data.Functor.Compose
import Graphics.Vty.Attributes
import Graphics.Vty (wcswidth)
import Data.Semigroup ((<>))

render :: Palette -> r -> Args r a -> String -> Image'
render pal r spec str = img2
  where
    (img, excess) = runState (getState (renderArgs pal r spec)) str
    img1 = img <> if any (' '/=) excess then string defAttr excess else mempty
    minLen = wcswidth str
    img2 | imageWidth img1 < minLen = resizeImage minLen img1
         | otherwise                = img1


renderArgs :: Palette -> r -> Args r a -> Renderer a
renderArgs pal r = runAp (renderArg pal r)

------------------------------------------------------------------------

type Renderer = Compose (State String) (Const Image')

getState :: Renderer a -> State String Image'
getState = fmap getConst . getCompose

putState :: State String Image' -> Renderer a
putState = Compose . fmap Const

------------------------------------------------------------------------

renderArg :: Palette -> r -> Arg r a -> Renderer b
renderArg pal r spec = putState $

  let placeholder name = return (" " <> string (view palCommandPlaceholder pal) name) in

  case spec of
    Optional subspec -> getState (renderArgs pal r subspec)

    Extension name ext ->
      do (lead,tok) <- state token
         if null tok then
           placeholder name
         else do
           rest <- case ext r tok of
                     Nothing      -> return mempty
                     Just subspec -> getState (renderArgs pal r subspec)
           return (string defAttr (lead++tok) <> rest)

    Argument TokenArgument name _ ->
      do (lead,tok) <- state token
         if null tok then
           placeholder name
         else
           return (string defAttr (lead++tok))

    Argument RemainingArgument name _ ->
      do rest <- state (\x -> (x,""))
         if all (' '==) rest then
           placeholder name
         else
           return (string defAttr rest)

token :: String -> ((String, String), String)
token xs =
  let (lead, xs1) = span  (' '==) xs
      (tok , xs2) = break (' '==) xs1
  in ((lead, tok), xs2)
