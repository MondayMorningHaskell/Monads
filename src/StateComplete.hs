module StateComplete where

import           Control.Monad.State
import qualified Data.Array as A
import qualified Data.Ix as I
import           System.Random (StdGen, randomR, newStdGen)

{- State Monad reference

get :: State s s
put :: s -> State s ()
runState :: State s a -> s -> (a, s)

-}

data Player = XPlayer | OPlayer

data TileState = Empty | HasX | HasO
  deriving (Show, Eq)

type TileIndex = (Int, Int)

boardIndices :: [TileIndex]
boardIndices = I.range ((0, 0), (2,2))

data GameState = GameState
  { board :: A.Array TileIndex TileState
  , currentPlayer :: Player
  , generator :: StdGen
  }

initialGameState :: StdGen -> GameState
initialGameState gen = GameState
  (A.array (head boardIndices, last boardIndices) [(i, Empty) | i <- boardIndices])
  XPlayer
  gen

nextPlayer :: Player -> Player
nextPlayer XPlayer = OPlayer
nextPlayer OPlayer = XPlayer

tileForPlayer :: Player -> TileState
tileForPlayer XPlayer = HasX
tileForPlayer OPlayer = HasO

chooseRandomMove :: State GameState TileIndex
chooseRandomMove = do
  game <- get
  let openSpots = [ fst pair | pair <- A.assocs (board game), snd pair == Empty]
  let gen = generator game
  let (i, gen') = randomR (0, length openSpots - 1) gen
  put $ game { generator = gen' }
  return $ openSpots !! i

applyMove :: TileIndex -> State GameState ()
applyMove i = do
  game <- get
  let p = currentPlayer game
  let newBoard = board game A.// [(i, tileForPlayer p)]
  put $ game { currentPlayer = nextPlayer p, board = newBoard }

isGameDone :: State GameState Bool
isGameDone = do
  game <- get
  let openSpots = [ fst pair | pair <- A.assocs (board game), snd pair == Empty]
  return $ length openSpots == 0

resolveTurn :: State GameState Bool
resolveTurn = do
  i <- chooseRandomMove
  applyMove i
  isGameDone
