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

render ::
  Palette  {- ^ palette             -} ->
  r        {- ^ environment         -} ->
  Bool     {- ^ render placeholders -} ->
  Args r a {- ^ specification       -} ->
  String   {- ^ user input          -} ->
  Image'
render pal env placeholders spec str = extend (addExcess img)
  where
    (img, excess) = flip runState str . getState
                  $ renderArgs pal env placeholders spec

    addExcess
      | any (' '/=) excess = (<> string defAttr excess)
      | otherwise          = id

    extend i
      | imageWidth i < minLen = resizeImage minLen i
      | otherwise             = i
      where minLen = wcswidth str



renderArgs :: Palette -> r -> Bool -> Args r a -> Renderer a
renderArgs pal r placeholders = runAp (renderArg pal r placeholders)

------------------------------------------------------------------------

type Renderer = Compose (State String) (Const Image')

getState :: Renderer a -> State String Image'
getState = fmap getConst . getCompose

putState :: State String Image' -> Renderer a
putState = Compose . fmap Const

------------------------------------------------------------------------

renderArg :: Palette -> r -> Bool -> Arg r a -> Renderer b
renderArg pal r placeholders spec = putState $

  let placeholder name
        | placeholders = return (" " <> string (view palCommandPlaceholder pal) name)
        | otherwise    = return mempty
  in

  case spec of
    Optional subspec -> getState (renderArgs pal r placeholders subspec)

    Extension name ext ->
      do (lead,tok) <- state token
         if null tok then
           placeholder name
         else do
           rest <- case ext r tok of
                     Nothing      -> return mempty
                     Just subspec -> getState (renderArgs pal r placeholders subspec)
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
