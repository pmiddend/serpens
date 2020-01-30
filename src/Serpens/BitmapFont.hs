{-# LANGUAGE TemplateHaskell #-}

module Serpens.BitmapFont where

import Control.Lens ((^.), (^..), folded, from, makeLenses, to)
import Data.Char (ord)
import Data.Text (Text)
import Data.Text.Lens (packed)
import Graphics.Gloss.Data.Bitmap (BitmapData)
import Graphics.Gloss.Data.Picture (Picture(Bitmap, BitmapSection))
import Graphics.Gloss.Juicy (loadJuicyPNG)
import Linear.V2 (V2(V2), _x)
import Serpens.Types
  ( IntPoint
  , Rectangle
  , SizedPicture(SizedPicture)
  , besides
  , mkRect
  , rectSize
  )
import Serpens.Util (betweenInclusive)

newtype BitmapFont =
  BitmapFont
    { _bitmapFont :: BitmapData
    }

makeLenses ''BitmapFont

letterSize :: IntPoint
letterSize = V2 12 14

mkLetterRect :: IntPoint -> Rectangle
mkLetterRect x = mkRect x letterSize

letterRect :: Char -> Char -> Int -> Rectangle
letterRect c c2 y =
  mkLetterRect (V2 (4 + (ord c - ord c2) * (letterSize ^. _x)) y)

letterToRectangle :: Char -> Maybe Rectangle
letterToRectangle c
  | betweenInclusive 'A' 'M' c = Just (letterRect c 'A' 4)
  | betweenInclusive 'N' 'Z' c = Just (letterRect c 'N' 24)
  | betweenInclusive 'a' 'm' c = Just (letterRect c 'a' 58)
  | betweenInclusive 'n' 'z' c = Just (letterRect c 'n' 78)
  | betweenInclusive '0' '9' c = Just (letterRect c '0' 112)
  | c == '+' = Just (mkRect (V2 188 18) letterSize)
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
loadBitmapFont fp = do
  result <- loadJuicyPNG fp
  case result of
    Just (Bitmap bmpData) -> pure (Right (BitmapFont bmpData))
    Just _ ->
      pure
        (Left $ "couldn't load bitmap font \"" <> fp <>
         "\": not a bitmap Picture")
    Nothing -> pure (Left "couldn't load image")

renderText :: BitmapFont -> Text -> SizedPicture
renderText font text =
  let pictures :: [SizedPicture]
      pictures =
        text ^.. from packed . folded . to (letterToSizedPicture font) . folded
   in foldr besides mempty pictures
