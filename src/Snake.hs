{-# LANGUAGE TemplateHaskell, FlexibleContexts #-}
{-# LANGUAGE DerivingStrategies #-}
module Snake where

import Control.Applicative ((<|>))
import Control.Monad (guard)
import Data.Maybe (fromMaybe)

import Data.Sequence (Seq, ViewL(..), ViewR(..), (<|))
import qualified Data.Sequence as S
import Lens.Micro.TH (makeLenses)
import Lens.Micro ((&), (.~), (%~), (^.))
import Linear.V2 (V2(..), _x, _y)
import System.Random (Random(..), newStdGen)

data Game = Game
  { _snake  :: Snake        -- ^ snake as a sequence of points in N2
  , _dir    :: Direction    -- ^ direction
  , _food   :: Coord        -- ^ location of the food
  , _foods  :: Stream Coord -- ^ infinite list of random next food locations
  , _dead   :: Bool         -- ^ game over flag
  , _paused :: Bool         -- ^ paused flag
  , _score  :: Int          -- ^ score
  , _locked :: Bool         -- ^ lock to disallow duplicate turns between time steps
  } deriving stock (Show)

type Coord = V2 Int
type Snake = Seq Coord

data Stream a = a :| Stream a
  deriving stock (Show)

data Direction =
  North
  | South
  | East
  | West
  deriving stock (Eq, Show)

makeLenses ''Game

height, width :: Int
height = 20
width = 20

step :: Game -> Game
step g = fromMaybe g $ do
  guard (not $ g ^. paused || g ^. dead)
  let g' = g & frozen .~ False
  return . fromMaybe (move g') $ die g' <|> eatFood g'

die :: Game -> Maybe Game

move :: Game -> Game

turn :: Direction -> Game -> Game

initGame :: IO Game