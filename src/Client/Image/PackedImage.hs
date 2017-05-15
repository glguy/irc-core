{-# Language OverloadedStrings #-}
{-|
Module      : Client.Image.PackedImage
Description : Packed vty Image type
Copyright   : (c) Eric Mertens, 2016
License     : ISC
Maintainer  : emertens@gmail.com

This module provides a more memory efficient way to store images.

-}
module Client.Image.PackedImage
  ( Image'
  , _Image'

  -- * Packed image construction
  , char
  , text'
  , string
  ) where

import           Control.Lens (Iso', iso)
import qualified Data.Text as S
import qualified Data.Text.Lazy as L
import           Data.Semigroup
import           Graphics.Vty.Attributes
import           Graphics.Vty.Image ((<|>), wcswidth, wcwidth)
import           Graphics.Vty.Image.Internal (Image(..))


-- | Isomorphism between packed images and normal images.
_Image' :: Iso' Image' Image
_Image' = iso unpackImage packImage
{-# INLINE _Image' #-}


unpackImage :: Image' -> Image
unpackImage EmptyImage'            = EmptyImage
unpackImage (HorizText' a b c d e) = HorizText a (L.fromStrict b) c d <|> unpackImage e

packImage :: Image -> Image'
packImage = flip go mempty
  where
    go EmptyImage          = id
    go (HorizText a b c d) = mappend (HorizText' a (L.toStrict b) c d EmptyImage')
    go (HorizJoin l r _ _) = go l . go r
    go _                   = mappend (text' (withForeColor defAttr red) "PANIC: packImage incomplete")


-- | Packed, strict version of 'Image' used for long-term storage of images.
data Image'
  = HorizText'
      Attr -- don't unpack, these get reused from the palette
      {-# UNPACK #-} !S.Text
      {-# UNPACK #-} !Int
      {-# UNPACK #-} !Int
      !Image'
  | EmptyImage'
  deriving (Show)

instance Monoid Image' where
  mempty  = EmptyImage'
  mappend = (<>)

instance Semigroup Image' where
  -- maintain compressed form
  HorizText' a b c d EmptyImage' <> HorizText' a' b' c' d' rest
    | a == a' = HorizText' a (b <> b') (c + c') (d + d') rest

  EmptyImage'          <> y = y
  HorizText' a b c d e <> y = HorizText' a b c d (e <> y)

text' :: Attr -> S.Text -> Image'
text' a s = HorizText' a s (wcswidth (S.unpack s)) (S.length s) EmptyImage'

char :: Attr -> Char -> Image'
char a c = HorizText' a (S.singleton c) (wcwidth c) 1 EmptyImage'

string :: Attr -> String -> Image'
string a s = HorizText' a t (wcswidth s) (S.length t) EmptyImage'
  where t = S.pack s
