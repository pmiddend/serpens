{-# LANGUAGE TemplateHaskell #-}

module Serpens.BitmapFont where

import Control.Lens (Getter, (^.), (^..), folded, from, makeLenses, to)
import Data.Char (ord)
import Data.Text (Text)
import Data.Text.Lens (packed)
import Graphics.Gloss.Data.Bitmap (BitmapData)
import Graphics.Gloss.Data.Picture (Picture(BitmapSection))
import Linear.V2 (V2(V2), _x)
import Serpens.Types
  ( IntPoint
  , Rectangle
  , SizedPicture(SizedPicture)
  , (|||)
  , mkRect
  , rectSize
  )
import Serpens.Util (betweenInclusive, loadBitmapData)

newtype BitmapFont =
  BitmapFont
    { _bitmapFont :: BitmapData
    }

makeLenses ''BitmapFont

fontHeight :: Getter BitmapFont Int
fontHeight = to (const 14)

fontWidth :: Getter BitmapFont Int
fontWidth = to (const 12)

letterSize :: IntPoint
letterSize = V2 12 18

mkLetterRect :: IntPoint -> Rectangle
mkLetterRect x = mkRect x letterSize

letterRect :: Char -> Char -> Int -> Rectangle
letterRect c c2 y =
  mkLetterRect (V2 (4 + (ord c - ord c2) * (letterSize ^. _x)) y)

letterToRectangle :: Char -> Maybe Rectangle
letterToRectangle c
  | betweenInclusive 'A' 'M' c = Just (letterRect c 'A' 0)
  | betweenInclusive 'N' 'Z' c = Just (letterRect c 'N' 20)
  | betweenInclusive 'a' 'm' c = Just (letterRect c 'a' 58)
  | betweenInclusive 'n' 'z' c = Just (letterRect c 'n' 78)
  | betweenInclusive '0' '9' c = Just (letterRect c '0' 112)
  | c == '+' = Just (mkRect (V2 188 18) letterSize)
  | c == '[' = Just (mkRect (V2 203 24) letterSize)
  | c == ']' = Just (mkRect (V2 213 24) letterSize)
  | c == ':' = Just (mkRect (V2 251 4) letterSize)
  | c == ' ' = Just (mkRect (V2 160 4) letterSize)
  | otherwise = Nothing

letterToSizedPicture :: BitmapFont -> Char -> Maybe SizedPicture
letterToSizedPicture font c =
  (\r ->
     SizedPicture
       (fromIntegral <$> (r ^. rectSize))
       (BitmapSection r (font ^. bitmapFont))) <$>
  letterToRectangle c

loadBitmapFont :: FilePath -> IO (Either String BitmapFont)
loadBitmapFont fp = (BitmapFont <$>) <$> loadBitmapData fp

renderText :: BitmapFont -> Text -> SizedPicture
renderText font text =
  let pictures :: [SizedPicture]
      pictures =
        text ^.. from packed . folded . to (letterToSizedPicture font) . folded
   in foldr (|||) mempty pictures
