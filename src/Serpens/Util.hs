module Serpens.Util where

import Control.Lens (Iso', iso)
import Graphics.Gloss.Data.Bitmap (BitmapData)
import Graphics.Gloss.Data.Picture (Picture(Bitmap, Translate))
import Graphics.Gloss.Juicy (loadJuicyPNG)
import Linear.V2 (V2(V2))

type Endo a = a -> a

intTranslate :: V2 Int -> Endo Picture
intTranslate v = floatTranslate (fromIntegral <$> v)

floatTranslate :: V2 Float -> Endo Picture
floatTranslate (V2 x y) = Translate x y

betweenInclusive :: Ord a => a -> a -> a -> Bool
betweenInclusive a b x = x >= a && x <= b

pair :: Iso' (V2 a) (a, a)
pair = iso (\(V2 x y) -> (x, y)) (uncurry V2)

loadBitmapData :: FilePath -> IO (Either String BitmapData)
loadBitmapData fp = do
  result <- loadJuicyPNG fp
  case result of
    Just (Bitmap bmpData) -> pure (Right bmpData)
    Just _ ->
      pure
        (Left $
         "couldn't load bitmap data \"" <> fp <> "\": not a bitmap Picture")
    Nothing -> pure (Left "couldn't load image")
