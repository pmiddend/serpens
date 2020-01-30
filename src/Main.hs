{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Chronos(now, Time, Timespan(getTimespan))
import Torsor(Torsor(difference))
import Numeric.Lens(multiplying)
import System.FilePath((</>))
import System.Exit(exitFailure, exitSuccess)
import Serpens.Util(pair, Endo, loadBitmapData)
import Serpens.BitmapFont(loadBitmapFont, BitmapFont, renderText, fontHeight, fontWidth)
import Data.Text.Lens(packed)
import Control.Lens
  ( Getter
  , (%~)
  , (#)
  , non
  , (&)
  , (+~)
  , Optic'
  , Const
  , makeLenses
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
import Graphics.Gloss (Display(InWindow), Picture(Bitmap), white, red, green)
import Graphics.Gloss.Data.Bitmap
  ( BitmapFormat(..)
  , PixelFormat(..)
  , RowOrder(..)
  , bitmapOfByteString
  , bitmapSize
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

data BaseData = BaseData {
    _bdFont :: BitmapFont
  , _bdClockIcon :: SizedPicture
  , _bdBackpackIcon :: SizedPicture
  }

makeLenses ''BaseData

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

initialModel :: Time -> Model
initialModel startTime =
  let player = Player (V2 10 100) right
      field =
        emptyField (V2 300 200) & fieldIx (player ^. playerPos) .
        bitAt playerBit .~
        True &
        fieldRect (mkRect (V2 200 10) (V2 80 100)) .
        bitAt goalBit .~
        True & fieldIndexList (moveIndices (V2 50 50) slanterPowerupIndices) . bitAt slanterBit .~ True
   in Model
        { _modelStart = startTime
        , _modelEnd = Nothing
        , _modelField = field
        , _modelPlayer = player
        , _modelWindowSize = Nothing
        , _modelGameState = GameStateRunning
        }

aspectScale :: FloatPoint -> FloatPoint -> V2 Float
aspectScale outer inner =
  let relationX :: Int
      relationX = if outer ^. _x < outer ^. _y
                  then round (outer ^. _x) `div` round (inner ^. _x)
                  else round (outer ^. _y) `div` round (inner ^. _y)
      scaleFactor :: Float
      scaleFactor = fromIntegral relationX
   in V2 scaleFactor scaleFactor

fromIntegral' :: Optic' (->) (Const Float) Int Float
fromIntegral' = to fromIntegral

modelToPicture :: BaseData -> Time -> Model -> Picture
modelToPicture bd currentTime m =
  case m ^. modelWindowSize of
    Nothing -> mempty
    Just windowSize ->
      let bmp = bd ^. bdFont
          fSize :: IntPoint
          fSize = m ^. modelField . fieldSize
          elapsed :: Timespan
          elapsed = (m ^. modelEnd . non currentTime) `difference` (m ^. modelStart)
          elapsedSeconds = getTimespan elapsed `div`  1000000000
          elapsedMinutes = elapsedSeconds `div` 60
          modSeconds = elapsedSeconds `mod` 60
          timeString = view packed $ (if elapsedMinutes < 10 then "0" else "") <> show elapsedMinutes <> ":" <> (if modSeconds < 10 then "0" else "") <> show modSeconds
          bitmap :: SizedPicture
          bitmap =
            colorVectorToPicture
              (fromIntegral <$> fSize)
              (m ^. modelField . fieldVector . toColorVector)
          topSpacing :: Int
          topSpacing = 4
          halfSpacer = spacer (V2 (bmp ^. fontWidth . fromIntegral') 0)
          fullSpacer = spacer (V2 (bmp ^. fontWidth . to fromIntegral . multiplying 2) 0)
          levelVerticalSpace = (windowSize ^. _y) - (bmp ^. fontHeight * 3) - topSpacing
          scale :: FloatPoint
          scale = aspectScale (fromIntegral <$> (windowSize & _y .~ levelVerticalSpace)) (fromIntegral <$> fSize)
          levelPicture = colored white (scaled scale bitmap)
          timeCounter = uniScaled 1.5 (colored white (bd ^. bdClockIcon)) ||| halfSpacer ||| uniScaled 2 (colored white (renderText bmp timeString))
          inventory = colored white (bd ^. bdBackpackIcon) ||| halfSpacer ||| uniScaled 2 (colored white (renderText bmp "[empty]"))
          topLine = timeCounter ||| fullSpacer ||| inventory
          gameoverFont =
            mwhen
              (has (modelGameState . _GameStateGameover) m)
              (uniScaled 5 (colored white (renderText bmp "GAME OVER")))
          wonFont =
            mwhen
              (has (modelGameState . _GameStateWon) m)
              (uniScaled 5 (colored red (renderText bmp "YOU WON")))
       in view spPicture (((topLine === spacer (V2 (fromIntegral topSpacing) 0)) === levelPicture) <> gameoverFont <> wonFont)

worldStep :: Float -> Model -> IO Model
worldStep _ m = do
  currentTime <- now
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
      newModelEnd = case (newGameState (m ^. modelGameState), m ^. modelEnd) of
        (_, Just x) -> Just x
        (GameStateGameover, Nothing) -> Just currentTime
        (GameStateWon, Nothing) -> Just currentTime
        _ -> Nothing
  pure (newModel & modelPlayer . playerDirection %~ newDirection & modelGameState %~ newGameState & modelEnd .~ newModelEnd)

loadSizedPicture :: FilePath -> IO (Either String SizedPicture)
loadSizedPicture fp = (makeSizedPicture <$>) <$> loadBitmapData fp
  where makeSizedPicture bmpData = SizedPicture (fromIntegral <$> (bitmapSize bmpData ^. from pair)) (Bitmap bmpData)


main :: IO ()
main = do
  bitmapFont <- loadBitmapFont ("data" </> "font.png")
  clock <- loadSizedPicture ("data" </> "clock.png")
  backpack <- loadSizedPicture ("data" </> "backpack.png")
  now' <- now
  case BaseData <$> bitmapFont <*> clock <*> backpack of
    Left e -> do
      putStrLn ("error loading data " <> e)
      exitFailure
    Right baseData ->
      let backgroundColor = green
          stepsPerSecond = 30
          model' = initialModel now'
          eventHandler (EventKey (SpecialKey KeyLeft) Down _ _) model =
            pure (model & modelPlayer . playerDirection %~ rotateDirLeft)
          eventHandler (EventKey (SpecialKey KeyRight) Down _ _) model =
            pure (model & modelPlayer . playerDirection %~ rotateDirRight)
          eventHandler (EventResize s) model =
            pure (model & modelWindowSize ?~ s ^. from pair)
          eventHandler (EventKey (SpecialKey KeyEsc) Down _ _) _ = exitSuccess
          eventHandler _ model = pure model
          displayMode = InWindow "serpens 1.0" (model' ^. modelField . fieldSize . pair) (10, 10)
          pictureConverter model = do
            newNow <- now
            pure (modelToPicture baseData newNow model)
      in playIO
         displayMode
         backgroundColor
         stepsPerSecond
         model'
         pictureConverter
         eventHandler
         worldStep
