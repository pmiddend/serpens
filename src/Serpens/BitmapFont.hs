{-# LANGUAGE TemplateHaskell #-}

module Serpens.BitmapFont where

import Control.Lens (Lens', (^.), (^..), folded, from, makeLenses, to)
import Data.Char (ord)
import Data.Text (Text)
import Data.Text.Lens (packed)
import Graphics.Gloss.Data.Bitmap (BitmapData)
import Graphics.Gloss.Data.Picture (Picture(Bitmap, BitmapSection))
import Graphics.Gloss.Juicy (loadJuicyPNG)
import Linear.V2 (V2(V2), _x, _y)
import Serpens.Types (Point, Rectangle, mkRect, rectSize)
import Serpens.Util (betweenInclusive, floatTranslate)

newtype BitmapFont =
  BitmapFont
    { _bitmapFont :: BitmapData
    }

makeLenses ''BitmapFont

data SizedPicture =
  SizedPicture
    { _spPicture :: Picture
    , _spSize :: Point
    }

emptySp :: SizedPicture
emptySp = SizedPicture mempty (V2 0 0)

makeLenses ''SizedPicture

spWidth :: Lens' SizedPicture Int
spWidth = spSize . _x

spHeight :: Lens' SizedPicture Int
spHeight = spSize . _y

besides :: SizedPicture -> SizedPicture -> SizedPicture
besides a b =
  let newSize' :: V2 Int
      newSize' =
        V2 (a ^. spWidth + b ^. spWidth) (max (a ^. spHeight) (b ^. spHeight))
      newSize :: V2 Float
      newSize = fromIntegral <$> newSize'
      newWidth = newSize ^. _x
      leftMove :: Float
      leftMove = (-(newWidth / 2) + fromIntegral (a ^. spWidth) / 2)
      rightMove :: Float
      rightMove = newWidth / 2 - fromIntegral (b ^. spWidth) / 2
      newPicture =
        floatTranslate (V2 leftMove 0) (a ^. spPicture) <>
        floatTranslate (V2 rightMove 0) (b ^. spPicture)
   in SizedPicture newPicture newSize'

letterSize :: Point
letterSize = V2 12 14

mkLetterRect :: Point -> Rectangle
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
  (\r -> SizedPicture (BitmapSection r (font ^. bitmapFont)) (r ^. rectSize)) <$>
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

renderText :: BitmapFont -> Text -> Picture
renderText font text =
  let pictures :: [SizedPicture]
      pictures =
        text ^.. from packed . folded . to (letterToSizedPicture font) . folded
   in foldr besides emptySp pictures ^. spPicture
