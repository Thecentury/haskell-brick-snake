{-# LANGUAGE OverloadedStrings #-}
module UI where

import Control.Monad (forever, void)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (threadDelay, forkIO)
import Data.Maybe (fromMaybe)

import Snake

import Brick
  (App(..), AttrMap, BrickEvent(..), EventM, Next, Widget,
  customMain, neverShowCursor,
  continue, halt,
  hLimit, vLimit, vBox, hBox,
  padRight, padLeft, padTop, padAll, Padding(..),
  withBorderStyle,
  str,
  attrMap, withAttr, emptyWidget, AttrName, on, fg,
  (<+>))
import Brick.BChan (newBChan, writeBChan)
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import qualified Graphics.Vty as V
import Data.Sequence (Seq)
import qualified Data.Sequence as S
import Linear.V2 (V2(..))
import Lens.Micro ((^.))

data Tick = Tick

type Name = ()

data Cell = Snake | Food | Empty

app :: App Game Tick Name
app = App
  { appDraw = drawUI
  , appChooseCursor = neverShowCursor
  , appHandleEvent = handleEvent
  , appStartEvent = return
  , appAttrMap = const theMap
  }

main :: IO ()
main = do
  chan <- newBChan 10
  tv <- atomically $ newTVar (spToInt initialSpped)
  forkIO $ forever $ do
    writeBChan chan Tick
    int <- atomically $ readTVar tv
    threadDelay int
  customMain (V.mkVty V.defaultConfig) (Just chan) app (initialGame tv)
    >>= printResult

handleEvent :: Game -> BrickEvent Name Tick -> EventM Name (Next Game)
handleEvent g (AppEvent Tick)                         = continue $ step g
handleEvent g (VtyEvent (V.EvKey V.KUp []))           = continue $ turn North g
handleEvent g (VtyEvent (V.EvKey V.KDown []))         = continue $ turn South g
handleEvent g (VtyEvent (V.EvKey V.KRight []))        = continue $ turn East g
handleEvent g (VtyEvent (V.EvKey V.KLeft []))         = continue $ turn West g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'k') []))   = continue $ turn North g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'j') []))   = continue $ turn South g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'l') []))   = continue $ turn East g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'h') []))   = continue $ turn West g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'r') []))   = liftIO initGame >>= continue
handleEvent g (VtyEvent (V.EvKey (V.KChar 'q') []))   = halt g
handleEvent g (VtyEvent (V.EvKey V.KEsc []))          = halt g
handleEvent g (VtyEvent (V.EvKey V.KRight [V.MCtrl])) = handleSpeed g (+)
handleEvent g (VtyEvent (V.EvKey V.KLeft [V.MCtrl]))  = handleSpeed g (-)
handleEvent g _                                       = continue g

handleSpeed :: Game -> (Float -> Float -> Float) -> EventM n (Next Game)
handleSpeed g (+/-) = do
  let newSp = validS $ (g ^. speed) +/- speedInc
  liftIO $ atomically $ writeTVar (g ^. interval) (spToInt newSp)
  continue $ g & speed .~ newSp

speedInc :: Float
speedInc = 0.01

data Game = Game
  { _board :: Board,
    _time :: Int,
    _paused :: Bool,
    _speed :: Float,
    _interval :: TVar Int,
    _focus :: F.FocusRing Name,
    _selected :: Cell }

drawUI :: Game -> [Widget Name]
drawUI g =
  [ C.center $ padRight (Pad 2) (drawStats g) <+> drawGrid g]

drawStats :: Game -> Windget Name
drawStats g = hLimit 11
  $ vBox [ drawScore (g ^. score)
         , padTop (Pad 2) $ drawGameOver (g ^. dead)
         ]

drawScore :: Int -> Widget Name
drawScore n = withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str "Score")
  $ C.hCenter
  $ padAll 1
  $ str $ show n

drawGameOver :: Bool -> Widget Name
drawGameOver dead =
  if dead
    then withAttr gameOverAttr $ C.hCenter $ str "GAME OVER"
    else emptyWidget

gameOverAttr :: AttrName
gameOverAttr = "gameOver"

drawGrid :: Game -> Widget Name
drawGrid g = withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str "Snake")
  $ vBox rows
  where
    rows = [hBox $ cellsInRow r | r <- [height - 1, height - 2..0]]
    cellsInRow y = [drawCoord (V2 x y) | x <- [0..width-1]]
    drawCoord = drawCell . cellAt
    cellAt c =
      | c `elem` g ^. snake = Snake
      | c == g ^. food      = Food
      | otherwise           = Empty

drawCell :: Cell -> Widget Name
drawCell Snake = withAttr snakeAttr cw
drawCell Food  = withAttr foodAttr cw
drawCell Empty = withAttr emptyAttr cw

cw :: Widget Name
cw = str "  "

snakeAttr, foodAttr, emptyAttr :: AttrName
snakeAttr = "snakeAttr"
foodAttr = "foodAttr"
emptyAttr = "emptyAttr"

theMap :: AttrMap
theMap = attrMap V.defAttr
  [ (snakeAttr, V.blue `on` V.blue),
    (foodAttr, V.red `on` V.red),
    (gameOverAttr, fg V.red `V.withStyle` V.bold)
  ]