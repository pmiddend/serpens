{-# LANGUAGE TemplateHaskell #-}

module Serpens.Pixure where

import Control.Lens (Iso', (^.), from, iso, makeLenses)
import qualified Data.ByteString as BS
import qualified Data.Vector.Storable as V
import Data.Vector.Storable.ByteString (byteStringToVector, vectorToByteString)
import Data.Word (Word8)
import qualified Graphics.Gloss.Data.Bitmap as Bitmap
import Graphics.Gloss.Data.Picture (Picture(Bitmap, BitmapSection))
import Graphics.Gloss.Juicy (loadJuicyPNG)
import Linear.V2 (V2(V2), _x, _y)
import Linear.Vector ((^/))
import Serpens.Util (floatTranslate, pair)

type FloatPoint = V2 Float

type IntPoint = V2 Int

data Rectangle a =
  Rectangle
    { _rectPos :: V2 a
    , _rectSize :: V2 a
    }

makeLenses ''Rectangle

bitmapRectangle :: Iso' (Rectangle Int) Bitmap.Rectangle
bitmapRectangle = iso toBitmap fromBitmap
  where
    fromBitmap :: Bitmap.Rectangle -> Rectangle Int
    fromBitmap r =
      Rectangle (Bitmap.rectPos r ^. from pair) (Bitmap.rectSize r ^. from pair)
    toBitmap :: Rectangle Int -> Bitmap.Rectangle
    toBitmap r = Bitmap.Rectangle (r ^. rectPos . pair) (r ^. rectSize . pair)

rootRect :: Num a => V2 a -> Rectangle a
rootRect = Rectangle (V2 0 0)

data BitmapSubrect =
  BitmapSubrect
    { _bsBitmap :: Bitmap.BitmapData
    , _bsRectangle :: Rectangle Int
    }

makeLenses ''BitmapSubrect

data PixureElement =
  PixureElement
    { _peBitmap :: BitmapSubrect
    , _peRect :: Rectangle Float
    }

makeLenses ''PixureElement

loadPixureElement :: FilePath -> IO (Either String PixureElement)
loadPixureElement fp = do
  result <- loadJuicyPNG fp
  case result of
    Just (Bitmap bmpData) ->
      let imageSize :: V2 Int
          imageSize = Bitmap.bitmapSize bmpData ^. from pair
       in pure
            (Right
               (PixureElement
                  (BitmapSubrect bmpData (rootRect imageSize))
                  (rootRect (fromIntegral <$> imageSize))))
    Just _ ->
      pure
        (Left $ "couldn't load picture \"" <> fp <> "\": not a bitmap Picture")
    Nothing -> pure (Left ("couldn't load image: " <> fp))

newtype Pixure =
  Pixure (IntPoint -> [PixureElement])

makeLenses ''Pixure

pixureElementToPicture :: IntPoint -> PixureElement -> Picture
pixureElementToPicture windowSize pe =
  let translation =
        (-(fromIntegral <$> windowSize) ^/ 2) + ((pe ^. peRect . rectSize) ^/ 2)
   in floatTranslate
        translation
        (BitmapSection
           (pe ^. peBitmap . bsRectangle . bitmapRectangle)
           (pe ^. peBitmap . bsBitmap))

scaled :: Pixure -> Pixure
scaled

pixureToPicture :: IntPoint -> Pixure -> Picture
pixureToPicture windowSize (Pixure f) =
  foldMap (pixureElementToPicture windowSize) (f windowSize)

type ColorVector = V.Vector Word8

colorVectorToPixure :: IntPoint -> ColorVector -> Pixure
colorVectorToPixure size v =
  Pixure $
  const
    [ PixureElement
        (BitmapSubrect (extractBitmapData bitmap) (rootRect size))
        (rootRect (fromIntegral <$> size))
    ]
  where
    bitmap =
      Bitmap.bitmapOfByteString
        (size ^. _x)
        (size ^. _y)
        (Bitmap.BitmapFormat Bitmap.TopToBottom Bitmap.PxRGBA)
        (v ^. fromVector)
        False
    extractBitmapData :: Picture -> Bitmap.BitmapData
    extractBitmapData (Bitmap bd) = bd
    extractBitmapData _ = error "not a bitmap"
    fromVector :: Iso' ColorVector BS.ByteString
    fromVector = iso vectorToByteString byteStringToVector
