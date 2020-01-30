{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Debug.Trace(traceShowId)
import System.FilePath((</>))
import System.Exit(exitFailure, exitSuccess)
import Serpens.Util(pair, Endo)
import Serpens.BitmapFont(loadBitmapFont, BitmapFont, renderText)
import Control.Lens
  ( Getter
  , (%~)
  , (#)
  , (&)
  , (+~)
  , view
  , (.~)
  , (?~)
  , (^.)
  , from
  , has
  , to
  )
import Data.Bits.Lens (bitAt)
import Data.Int (Int64)
import qualified Data.Vector.Storable as V
import Data.Word (Word8)
import Graphics.Gloss (Display(InWindow), Picture, white, red, black)
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

mwhen :: Monoid m => Bool -> m -> m
mwhen True m = m
mwhen False _ = mempty

colorVectorToPicture :: IntPoint -> ColorVector -> SizedPicture
colorVectorToPicture size f = SizedPicture (fromIntegral <$> size) $
  bitmapOfByteString
    (size ^. _x)
    (size ^. _y)
    (BitmapFormat TopToBottom PxRGBA)
    (f ^. fromVector)
    False

playerBit :: Int
playerBit = 0

goalBit :: Int
goalBit = 1

slanterBit :: Int
slanterBit = 2

emptyField :: IntPoint -> Field
emptyField s = Field s (V.replicate (s ^. _x * s ^. _y * 4) 0)

vWhite :: V.Vector Word8
vWhite = V.fromList [255, 255, 255, 255]

vBlack :: V.Vector Word8
vBlack = V.fromList [0, 0, 0, 255]

vRed :: V.Vector Word8
vRed = V.fromList [255, 0, 0, 255]

vGreen :: V.Vector Word8
vGreen = V.fromList [0, 255, 0, 255]

vBlue :: V.Vector Word8
vBlue = V.fromList [0, 0, 255, 255]

toColorVector :: Getter FieldVector ColorVector
toColorVector =
  let processElement :: Int64 -> V.Vector Word8
      processElement e
        | e ^. bitAt playerBit = vGreen
        | e ^. bitAt goalBit = vRed
        | e ^. bitAt slanterBit = vBlue
        | otherwise = vBlack
   in to (V.concatMap processElement)

slanterPowerupIndices :: [IntPoint]
slanterPowerupIndices = [V2 0 0, V2 2 0, V2 1 1, V2 0 2, V2 2 2]

moveIndices :: IntPoint -> Endo [IntPoint]
moveIndices v ps = (v +) <$> ps

initialModel :: Model
initialModel =
  let player = Player (V2 10 100) right
      field =
        emptyField (V2 300 200) & fieldIx (player ^. playerPos) .
        bitAt playerBit .~
        True &
        fieldRect (mkRect (V2 200 10) (V2 80 100)) .
        bitAt goalBit .~
        True & fieldIndexList (moveIndices (V2 50 50) slanterPowerupIndices) . bitAt slanterBit .~ True
   in Model
        { _modelField = field
        , _modelPlayer = player
        , _modelWindowSize = Nothing
        , _modelGameState = GameStateRunning
        }

aspectScale :: FloatPoint -> FloatPoint -> V2 Float
aspectScale outer inner =
  let relationX :: Int
      relationX = round (outer ^. _x) `div` round (inner ^. _x)
      scaleFactor :: Float
      scaleFactor = fromIntegral relationX
   in V2 scaleFactor scaleFactor

modelToPicture :: BitmapFont -> Model -> Picture
modelToPicture bmp m =
  case m ^. modelWindowSize of
    Nothing -> mempty
    Just windowSize ->
      let bitmapSize :: IntPoint
          bitmapSize = m ^. modelField . fieldSize
          bitmap :: SizedPicture
          bitmap =
            colorVectorToPicture
              (fromIntegral <$> bitmapSize)
              (m ^. modelField . fieldVector . toColorVector)
          scale :: FloatPoint
          scale = aspectScale (fromIntegral <$> windowSize) (fromIntegral <$> bitmapSize)
          levelPicture = scaleSp scale bitmap
          timeCounter = scaleSp (V2 2 2) (colorSp white (renderText bmp "TIME 00:00"))
          gameoverFont =
            mwhen
              (has (modelGameState . _GameStateGameover) m)
              (scaleSp (V2 5 5) (colorSp white (renderText bmp "GAME OVER")))
          wonFont =
            mwhen
              (has (modelGameState . _GameStateWon) m)
              (scaleSp (V2 5 5) (colorSp red (renderText bmp "YOU WON")))
       in view spPicture ((timeCounter `above` levelPicture) `onTop` gameoverFont `onTop` wonFont)

worldStep :: Float -> Model -> IO Model
worldStep _ m =
  let playerDirection' = m ^. modelPlayer . playerDirection
      playerMoving = playerDirection' /= noDirection
      newPlayer =
        (m ^. modelPlayer) & playerPos +~ (directionPoint # playerDirection')
      playerValue :: Int64
      playerValue = m ^. modelField . fieldIx (newPlayer ^. playerPos)
      playerHitsSelf = playerMoving && playerValue ^. bitAt playerBit
      playerHitsGoal = playerValue ^. bitAt goalBit
      newModel =
        m & modelPlayer .~ newPlayer & modelField .
        fieldIx (newPlayer ^. playerPos) .
        bitAt playerBit .~
        True
      newDirection dir =
        if playerHitsSelf || playerHitsGoal
          then noDirection
          else dir
      newGameState state
        | playerHitsSelf = GameStateGameover
        | playerHitsGoal = GameStateWon
        | otherwise = state
   in pure
        (newModel & modelPlayer . playerDirection %~ newDirection &
         modelGameState %~
         newGameState)

main :: IO ()
main = do
  bitmapFont' <- loadBitmapFont ("data" </> "font.png")
  case bitmapFont' of
    Left e -> do
      putStrLn ("error loading font " <> e)
      exitFailure
    Right bitmapFont ->
      let backgroundColor = white
          stepsPerSecond = 30
          eventHandler (EventKey (SpecialKey KeyLeft) Down _ _) model =
            pure (model & modelPlayer . playerDirection %~ rotateDirLeft)
          eventHandler (EventKey (SpecialKey KeyRight) Down _ _) model =
            pure (model & modelPlayer . playerDirection %~ rotateDirRight)
          eventHandler (EventResize s) model =
            pure (model & modelWindowSize ?~ s ^. from pair)
          eventHandler (EventKey (SpecialKey KeyEsc) Down _ _) _ = exitSuccess
          eventHandler _ model = pure model
          displayMode = InWindow "serpens 1.0" (initialModel ^. modelField . fieldSize . pair) (10, 10)
      in playIO
         displayMode
         backgroundColor
         stepsPerSecond
         initialModel
         (pure . modelToPicture bitmapFont)
         eventHandler
         worldStep
