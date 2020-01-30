{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Rank2Types #-}

module Serpens.Types where

import Control.Lens
  ( Iso'
  , Lens'
  , Prism'
  , (&)
  , over
  , Traversal'
  , (<>~)
  , Iso
  , to
  , Getter
  , (.~)
  , from
  , traversed
  , indices
  , re
  , (#)
  , (^.)
  , (^?)
  , (^?!)
  , iso
  , ix
  , makeLenses
  , makePrisms
  , prism'
  , to
  )
import qualified Data.ByteString as BS
import Data.Int (Int64)
import qualified Data.Vector.Storable as V
import Graphics.Gloss.Data.Picture (Picture(Color, Scale))
import Graphics.Gloss.Data.Color (Color)
import qualified Graphics.Gloss.Data.Bitmap as Bitmap
import Data.Vector.Storable.ByteString (byteStringToVector, vectorToByteString)
import Data.Word (Word8)
import Linear.V2 (V2(..), _x, _y, perp)
import Serpens.Util (Endo, pair, betweenInclusive, floatTranslate)


type IntPoint = V2 Int

type FloatPoint = V2 Float

type FieldVector = V.Vector Int64

type ColorVector = V.Vector Word8

type StepType = Int

data SizedPicture =
  SizedPicture
    { _spSize :: FloatPoint
    , _spPicture :: Picture
    }

makeLenses ''SizedPicture

onTop :: SizedPicture -> SizedPicture -> SizedPicture
onTop a b = a & spPicture <>~ (b ^. spPicture)

instance Semigroup SizedPicture where
  (<>) = onTop

instance Monoid SizedPicture where
  mempty = SizedPicture (V2 0 0) mempty


scaleSp :: FloatPoint -> Endo SizedPicture
scaleSp v = over spPicture (Scale (v ^. _x) (v ^. _y)) . over spSize (v *)

colorSp :: Color -> Endo SizedPicture
colorSp c = over spPicture (Color c)


spWidth :: Lens' SizedPicture Float
spWidth = spSize . _x

spHeight :: Lens' SizedPicture Float
spHeight = spSize . _y

besides :: SizedPicture -> SizedPicture -> SizedPicture
besides a b =
  let newSize :: V2 Float
      newSize =
        V2 (a ^. spWidth + b ^. spWidth) (max (a ^. spHeight) (b ^. spHeight))
      newWidth = newSize ^. _x
      leftMove :: Float
      leftMove = (-(newWidth / 2) + (a ^. spWidth) / 2)
      rightMove :: Float
      rightMove = newWidth / 2 - (b ^. spWidth) / 2
      newPicture =
        floatTranslate (V2 leftMove 0) (a ^. spPicture) <>
        floatTranslate (V2 rightMove 0) (b ^. spPicture)
   in SizedPicture newSize newPicture

above :: SizedPicture -> SizedPicture -> SizedPicture
above a b =
  let newSize = V2 (max (a ^. spWidth) (b ^. spWidth)) (a ^. spHeight + b ^. spHeight)
      newA = floatTranslate (V2 0 (-(b ^. spHeight)/2 - (a ^. spHeight)/2)) (a ^. spPicture)
      newPicture = newA <> (b ^. spPicture) 
    in SizedPicture newSize newPicture

fromVector :: Iso' ColorVector BS.ByteString
fromVector = iso vectorToByteString byteStringToVector

data Field =
  Field
    { _fieldSize :: !IntPoint
    , _fieldVector :: !FieldVector
    }

makeLenses ''Field

fieldWidth :: Lens' Field Int
fieldWidth = fieldSize . _x

fieldHeight :: Lens' Field Int
fieldHeight = fieldSize . _y

fromFieldIndex :: Field -> IntPoint -> Int
fromFieldIndex field (V2 x' y') =
  let x = x' `mod` (field ^. fieldWidth)
      y = y' `mod` (field ^. fieldHeight)
  in (field ^. fieldWidth * y) + x

toFieldIndex :: Field -> Int -> IntPoint
toFieldIndex field i =
  let x = i `mod` (field ^. fieldWidth)
      y = i `div` (field ^. fieldWidth)
  in V2 x y

fieldIx :: IntPoint -> Lens' Field Int64
fieldIx v f field =
  let vi = fromFieldIndex field v
      helper :: Int64 -> Field
      helper i = field & fieldVector . ix vi .~ i
   in helper <$> f (field ^?! fieldVector . ix vi)

type Rectangle = Bitmap.Rectangle

mkRect :: IntPoint -> IntPoint -> Rectangle
mkRect pos size = Bitmap.Rectangle {
    Bitmap.rectPos = pos ^. pair
  , Bitmap.rectSize = size ^. pair
  }

rootRect :: IntPoint -> Rectangle
rootRect = mkRect (V2 0 0)

rectPos :: Lens' Rectangle IntPoint
rectPos f r =
  let (x, y) = Bitmap.rectPos r
      changePos :: Rectangle -> IntPoint -> Rectangle
      changePos _ p = r { Bitmap.rectPos = p ^. pair }
  in changePos r <$> f (V2 x y)

rectSize :: Lens' Rectangle IntPoint
rectSize f r =
  let (x, y) = Bitmap.rectSize r
      changeSize :: Rectangle -> IntPoint -> Rectangle
      changeSize r' p = r' { Bitmap.rectSize = p ^. pair }
  in changeSize r <$> f (V2 x y)

rectTopLeft :: Getter Rectangle IntPoint
rectTopLeft = to (\r -> V2 (r ^. rectPos . _x) (r ^. rectPos . _y))

rectBottomRight :: Getter Rectangle IntPoint
rectBottomRight = to (\r -> V2 (r ^. rectPos . _x + r ^. rectSize . _x) (r ^. rectPos . _y + r ^. rectSize . _y))

rectLeft :: Getter Rectangle Int
rectLeft = rectPos . _x

rectRight :: Getter Rectangle Int
rectRight = rectBottomRight . _x

rectTop :: Getter Rectangle Int
rectTop = rectTopLeft . _y

rectBottom :: Getter Rectangle Int
rectBottom = rectBottomRight . _y

pointInRect :: Rectangle -> IntPoint -> Bool
pointInRect r p = betweenInclusive (r ^. rectLeft) (r ^. rectRight) (p ^. _x) &&
  betweenInclusive (r ^. rectTop) (r ^. rectBottom) (p ^. _y)

vector :: (V.Storable a, V.Storable b) => Iso [a] [b] (V.Vector a) (V.Vector b)
vector = iso V.fromList V.toList

fieldRect :: Rectangle -> Traversal' Field Int64
fieldRect r f field = (fieldVector . from vector . traversed . indices ((r `pointInRect`) . toFieldIndex field)) f field

fieldIndexList :: [IntPoint] -> Traversal' Field Int64
fieldIndexList ps f field = (fieldVector . from vector . traversed . indices ((`elem` ps) . toFieldIndex field)) f field


data Direction1D
  = Negative
  | Zero
  | Positive
  deriving(Eq)

newtype Direction2D =
  Direction2D
    { _getDirection2D :: V2 Direction1D
    } deriving(Eq)

makeLenses ''Direction2D

left, right, up, down, noDirection :: Direction2D
left = Direction2D (V2 Negative Zero)

right = Direction2D (V2 Positive Zero)

up = Direction2D (V2 Zero Negative)

down = Direction2D (V2 Zero Positive)

noDirection = Direction2D (V2 Zero Zero)

directionInt :: Prism' Int Direction1D
directionInt = prism' toInt fromInt
  where
    toInt :: Direction1D -> Int
    toInt Negative = -1
    toInt Positive = 1
    toInt Zero = 0
    fromInt :: Int -> Maybe Direction1D
    fromInt 0 = Just Zero
    fromInt 1 = Just Positive
    fromInt (-1) = Just Negative
    fromInt _ = Nothing

directionPoint :: Prism' IntPoint Direction2D
directionPoint = prism' toPoint fromPoint
  where
    toPoint :: Direction2D -> IntPoint
    toPoint (Direction2D v) = (directionInt #) <$> v
    fromPoint :: IntPoint -> Maybe Direction2D
    fromPoint (V2 x y) =
      Direction2D <$> (V2 <$> (x ^? directionInt) <*> (y ^? directionInt))

rotateDirRight :: Endo Direction2D
rotateDirRight dir = dir ^?! re directionPoint . to perp . directionPoint

rotateDirLeft :: Endo Direction2D
rotateDirLeft dir = iterate rotateDirRight dir !! 3

data Player =
  Player
    { _playerPos :: !IntPoint
    , _playerDirection :: !Direction2D
    }

makeLenses ''Player

data GameState
  = GameStateRunning
  | GameStateGameover
  | GameStateWon

makePrisms ''GameState

data Model =
  Model
    { _modelField :: !Field
    , _modelPlayer :: !Player
    , _modelWindowSize :: Maybe IntPoint
    , _modelGameState :: GameState
    }

makeLenses ''Model
