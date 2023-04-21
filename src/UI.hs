{-# LANGUAGE OverloadedStrings #-}
module UI (main) where

import Control.Monad.State.Strict
import Control.Concurrent (threadDelay, forkIO)

import Snake

import Brick
  ( App(..), AttrMap, BrickEvent(..), EventM, Widget
  , customMain, neverShowCursor
  , halt
  , hLimit, vBox, hBox  , padRight, padTop, padAll, Padding(..)
  , withBorderStyle, str
  , attrMap, withAttr, emptyWidget, AttrName, on, fg
  , (<+>), attrName
  )
import Brick.BChan (newBChan, writeBChan)
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import Control.Lens ((^.), (&), (.~))
import qualified Graphics.Vty as V
import Linear.V2 (V2(..))
import Control.Concurrent.STM (newTVar, readTVar, writeTVar, atomically)

-- Types

-- | Ticks mark passing of time
--
-- This is our custom event that will be constantly fed into the app.
data Tick = Tick

-- | Named resources
--
-- Not currently used, but will be easier to refactor
-- if we call this "Name" now.
type Name = ()

data Cell = Snake | Food | Empty

-- App definition

app :: App Game Tick Name
app = App { appDraw = drawUI
          , appChooseCursor = neverShowCursor
          , appHandleEvent = handleEvent
          , appStartEvent = return ()
          , appAttrMap = const theMap
          }

initialDelay :: Int
initialDelay = 100000

main :: IO ()
main = do
  chan <- newBChan 10
  intervalVar <- atomically $ newTVar initialDelay
  _ <- forkIO $ forever $ do
    writeBChan chan Tick
    delay <- atomically $ readTVar intervalVar
    threadDelay delay -- decides how fast your game moves
  game <- initGame intervalVar
  let builder = V.mkVty V.defaultConfig
  initialVty <- builder
  void $ customMain initialVty builder (Just chan) app game

-- Handling events

handleEvent :: BrickEvent Name Tick -> EventM Name Game ()
handleEvent (AppEvent Tick)                         = modify step
handleEvent (VtyEvent (V.EvKey V.KRight [V.MCtrl])) = handleSpeed (+)
handleEvent (VtyEvent (V.EvKey V.KLeft [V.MCtrl]))  = handleSpeed (-)
handleEvent (VtyEvent (V.EvKey V.KUp []))           = modify $ turn North
handleEvent (VtyEvent (V.EvKey V.KDown []))         = modify $ turn South
handleEvent (VtyEvent (V.EvKey V.KRight []))        = modify $ turn East
handleEvent (VtyEvent (V.EvKey V.KLeft []))         = modify $ turn West
handleEvent (VtyEvent (V.EvKey (V.KChar 'k') []))   = modify $ turn North
handleEvent (VtyEvent (V.EvKey (V.KChar 'j') []))   = modify $ turn South
handleEvent (VtyEvent (V.EvKey (V.KChar 'l') []))   = modify $ turn East
handleEvent (VtyEvent (V.EvKey (V.KChar 'h') []))   = modify $ turn West
handleEvent (VtyEvent (V.EvKey (V.KChar 'r') []))   = do
  game <- get
  newGame <- liftIO (initGame $ game ^. interval)
  put newGame
handleEvent (VtyEvent (V.EvKey (V.KChar 'q') []))   = halt
handleEvent (VtyEvent (V.EvKey V.KEsc []))          = halt
handleEvent _                                       = return ()

handleSpeed :: (Float -> Float -> Float) -> EventM Name Game ()
handleSpeed (+/-) = do
  g <- get
  let newSpeed = validSpeed $ (g ^. speed) +/- speedInc
  _ <- liftIO $ atomically $ writeTVar (g ^. interval) (speedToInt newSpeed)
  put $ g & speed .~ newSpeed

-- | Speed increments = 0.01 gives 100 discrete speed settings
speedInc :: Float
speedInc = 0.1

validSpeed :: Float -> Float
validSpeed s = min 40 (max 0.01 s)

speedToInt :: Float -> Int
speedToInt s = fromEnum (s / 0.01) * 100

-- Drawing

drawUI :: Game -> [Widget Name]
drawUI g =
  [ C.center $ padRight (Pad 2) (drawStats g) <+> drawGrid g ]

drawStats :: Game -> Widget Name
drawStats g = hLimit 11
  $ vBox [ drawScore (g ^. score)
         , drawSpeed (g ^. speed)
         , padTop (Pad 2) $ drawGameOver (g ^. dead)
         ]

drawSpeed :: Float -> Widget Name
drawSpeed s = withBorderStyle BS.unicodeRounded
  $ B.borderWithLabel (str "Speed")
  $ C.hCenter
  $ padAll 1
  $ str $ show s

drawScore :: Int -> Widget Name
drawScore n = withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str "Score")
  $ C.hCenter
  $ padAll 1
  $ str $ show n

drawGameOver :: Bool -> Widget Name
drawGameOver isDead =
  if isDead
     then withAttr gameOverAttr $ C.hCenter $ str "GAME OVER"
     else emptyWidget

drawGrid :: Game -> Widget Name
drawGrid g = withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str "Snake")
  $ vBox rows
  where
    rows         = [hBox $ cellsInRow r | r <- [height-1, height-2 .. 0]]
    cellsInRow y = [drawCoord (V2 x y) | x <- [0 .. width-1]]
    drawCoord    = drawCell . cellAt
    cellAt c
      | c `elem` g ^. snake = Snake
      | c == g ^. food      = Food
      | otherwise           = Empty

drawCell :: Cell -> Widget Name
drawCell Snake = withAttr snakeAttr cw
drawCell Food  = withAttr foodAttr cw
drawCell Empty = withAttr emptyAttr cw

cw :: Widget Name
cw = str "  "

theMap :: AttrMap
theMap = attrMap V.defAttr
  [ (snakeAttr, V.blue `on` V.blue)
  , (foodAttr, V.red `on` V.red)
  , (gameOverAttr, fg V.red `V.withStyle` V.bold)
  ]

gameOverAttr :: AttrName
gameOverAttr = attrName "gameOver"

snakeAttr, foodAttr, emptyAttr :: AttrName
snakeAttr = attrName "snakeAttr"
foodAttr = attrName "foodAttr"
emptyAttr = attrName "emptyAttr"