{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Rank2Types #-}

module Serpens.Types where

import Control.Lens
  ( Iso'
  , Lens'
  , Prism'
  , (&)
  , Traversal'
  , Iso
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
import Data.Vector.Storable.ByteString (byteStringToVector, vectorToByteString)
import Data.Word (Word8)
import Linear.V2 (V2(..), _x, _y, perp)
import Serpens.Util (Endo)

type Point = V2 Int

type FieldVector = V.Vector Int64

type ColorVector = V.Vector Word8

type StepType = Int

fromVector :: Iso' ColorVector BS.ByteString
fromVector = iso vectorToByteString byteStringToVector

data Field =
  Field
    { _fieldSize :: !Point
    , _fieldVector :: !FieldVector
    }

makeLenses ''Field

fieldWidth :: Lens' Field Int
fieldWidth = fieldSize . _x

fieldHeight :: Lens' Field Int
fieldHeight = fieldSize . _y

fromFieldIndex :: Field -> Point -> Int
fromFieldIndex field (V2 x' y') =
  let x = x' `mod` (field ^. fieldWidth)
      y = y' `mod` (field ^. fieldHeight)
  in (field ^. fieldWidth * y) + x

toFieldIndex :: Field -> Int -> Point
toFieldIndex field i =
  let x = i `mod` (field ^. fieldWidth)
      y = i `div` (field ^. fieldWidth)
  in V2 x y

fieldIx :: Point -> Lens' Field Int64
fieldIx v f field =
  let vi = fromFieldIndex field v
      helper :: Int64 -> Field
      helper i = field & fieldVector . ix vi .~ i
   in helper <$> f (field ^?! fieldVector . ix vi)

data Rectangle = Rectangle {
    _rectTopLeft :: Point
  , _rectBottomRight :: Point
  } deriving(Eq)

makeLenses ''Rectangle

rectLeft :: Lens' Rectangle Int
rectLeft = rectTopLeft . _x

rectRight :: Lens' Rectangle Int
rectRight = rectBottomRight . _x

rectTop :: Lens' Rectangle Int
rectTop = rectTopLeft . _y

rectBottom :: Lens' Rectangle Int
rectBottom = rectBottomRight . _y

betweenInclusive :: Ord a => a -> a -> a -> Bool
betweenInclusive a b x = x >= a && x <= b

pointInRect :: Rectangle -> Point -> Bool
pointInRect r p = betweenInclusive (r ^. rectLeft) (r ^. rectRight) (p ^. _x) &&
  betweenInclusive (r ^. rectTop) (r ^. rectBottom) (p ^. _y)

vector :: (V.Storable a, V.Storable b) => Iso [a] [b] (V.Vector a) (V.Vector b)
vector = iso V.fromList V.toList

fieldRect :: Rectangle -> Traversal' Field Int64
fieldRect r f field = (fieldVector . from vector . traversed . indices ((r `pointInRect`) . toFieldIndex field)) f field

data Direction1D
  = Negative
  | Zero
  | Positive

newtype Direction2D =
  Direction2D
    { _getDirection2D :: V2 Direction1D
    }

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

directionPoint :: Prism' Point Direction2D
directionPoint = prism' toPoint fromPoint
  where
    toPoint :: Direction2D -> Point
    toPoint (Direction2D v) = (directionInt #) <$> v
    fromPoint :: Point -> Maybe Direction2D
    fromPoint (V2 x y) =
      Direction2D <$> (V2 <$> (x ^? directionInt) <*> (y ^? directionInt))

rotateDirRight :: Endo Direction2D
rotateDirRight dir = dir ^?! re directionPoint . to perp . directionPoint

rotateDirLeft :: Endo Direction2D
rotateDirLeft dir = iterate rotateDirRight dir !! 3

data Player =
  Player
    { _playerPos :: !Point
    , _playerDirection :: !Direction2D
    }

makeLenses ''Player

data GameState
  = GameStateRunning
  | GameStateGameover

makePrisms ''GameState

data Model =
  Model
    { _modelField :: !Field
    , _modelPlayer :: !Player
    , _modelWindowSize :: Maybe Point
    , _modelGameState :: GameState
    }

makeLenses ''Model
