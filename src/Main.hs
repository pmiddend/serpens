{-# LANGUAGE Rank2Types #-}

module Main where

import Control.Applicative (liftA2)
import Control.Lens
  ( Getter
  , Iso'
  , Traversal
  , (%~)
  , (&)
  , (+~)
  , (.~)
  , (?~)
  , (^.)
  , from
  , has
  , iso
  , re
  , to
  )
import Data.Bits.Lens (bitAt)
import Data.Int (Int64)
import qualified Data.Vector.Storable as V
import Data.Word (Word8)
import Graphics.Gloss
  ( Display(InWindow)
  , Picture(Color, Pictures, Scale, Text)
  , white
  )
import Graphics.Gloss.Data.Bitmap
  ( BitmapFormat(..)
  , PixelFormat(..)
  , RowOrder(..)
  , bitmapOfByteString
  )
import Graphics.Gloss.Interface.IO.Game
  ( Event(EventKey, EventResize)
  , Key(SpecialKey)
  , KeyState(Down)
  , SpecialKey(KeyEsc, KeyLeft, KeyRight)
  , playIO
  )
import Linear.V2 (V2(..), _x, _y)
import Serpens.Types
import System.Exit (exitSuccess)

mwhen :: Monoid m => Bool -> m -> m
mwhen True m = m
mwhen False _ = mempty

colorVectorToPicture :: Point -> ColorVector -> Picture
colorVectorToPicture size f =
  bitmapOfByteString
    (size ^. _x)
    (size ^. _y)
    (BitmapFormat TopToBottom PxRGBA)
    (f ^. fromVector)
    False

playerBit :: Int
playerBit = 0

emptyField :: Point -> Field
emptyField s = Field s (V.replicate (s ^. _x * s ^. _y * 4) 0)

vWhite :: V.Vector Word8
vWhite = V.fromList [255, 255, 255, 255]

vBlack :: V.Vector Word8
vBlack = V.fromList [0, 0, 0, 255]

vRed :: V.Vector Word8
vRed = V.fromList [255, 0, 0, 255]

vGreen :: V.Vector Word8
vGreen = V.fromList [0, 255, 0, 255]

toColorVector :: Getter FieldVector ColorVector
toColorVector =
  let processElement :: Int64 -> V.Vector Word8
      processElement e
        | e ^. bitAt playerBit = vGreen
        | otherwise = vBlack
   in to (V.concatMap processElement)

initialModel :: Model
initialModel =
  let player = Player (V2 100 100) right
      field =
        emptyField (V2 300 200) & fieldIx (player ^. playerPos) .
        bitAt playerBit .~
        True
   in Model
        { _modelField = field
        , _modelPlayer = player
        , _modelWindowSize = Nothing
        , _modelGameState = GameStateRunning
        }

aspectScale :: Point -> Point -> V2 Float
aspectScale outer inner =
  let relationX :: Int
      relationX = (outer ^. _x) `div` (inner ^. _x)
      scaleFactor :: Float
      scaleFactor = fromIntegral relationX
   in V2 scaleFactor scaleFactor

modelToPicture :: Model -> Picture
modelToPicture m =
  case m ^. modelWindowSize of
    Nothing -> mempty
    Just windowSize ->
      let bitmapSize :: Point
          bitmapSize = m ^. modelField . fieldSize
          bitmap :: Picture
          bitmap =
            colorVectorToPicture
              bitmapSize
              (m ^. modelField . fieldVector . toColorVector)
          scale :: V2 Float
          scale = aspectScale windowSize bitmapSize
          levelPicture = Scale (scale ^. _x) (scale ^. _y) bitmap
          gameoverFont =
            mwhen
              (has (modelGameState . _GameStateGameover) m)
              (Scale 0.5 0.5 (Color white (Text "GAME OVER")))
       in levelPicture <> gameoverFont

pair :: Iso' (V2 a) (a, a)
pair = iso (\(V2 x y) -> (x, y)) (uncurry V2)

worldStep :: Float -> Model -> IO Model
worldStep _ m =
  let newPlayer =
        (m ^. modelPlayer) & playerPos +~
        (m ^. modelPlayer . playerDirection . re directionPoint)
      playerHitsSelf =
        m ^. modelField . fieldIx (newPlayer ^. playerPos) . bitAt playerBit
      newModel =
        m & modelPlayer .~ newPlayer & modelField .
        fieldIx (newPlayer ^. playerPos) .
        bitAt playerBit .~
        True
   in if playerHitsSelf
        then pure
               (newModel & modelPlayer . playerDirection .~ noDirection &
                modelGameState .~
                GameStateGameover)
        else pure newModel

main :: IO ()
main =
  let backgroundColor = white
      stepsPerSecond = 30
      eventHandler (EventKey (SpecialKey KeyLeft) Down _ _) model =
        pure (model & modelPlayer . playerDirection %~ rotateDirLeft)
      eventHandler (EventKey (SpecialKey KeyRight) Down _ _) model =
        pure (model & modelPlayer . playerDirection %~ rotateDirRight)
      eventHandler (EventResize s) model = do
        putStrLn ("window size " <> show s)
        pure (model & modelWindowSize ?~ s ^. from pair)
      eventHandler (EventKey (SpecialKey KeyEsc) Down _ _) _ = exitSuccess
      eventHandler _ model = pure model
      displayMode =
        InWindow
          "Nice Window"
          (initialModel ^. modelField . fieldSize . pair)
          (10, 10)
   in playIO
        displayMode
        backgroundColor
        stepsPerSecond
        initialModel
        (pure . modelToPicture)
        eventHandler
        worldStep
