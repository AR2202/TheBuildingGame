module Game where
import Data.Array
import Data.Tuple


data Winning = Winning|Losing  deriving (Eq, Show)
data Player = PlayerX | PlayerO | PlayerY | PlayerW deriving (Eq, Show)
type Cell = Maybe Player
data State = Running | GameOver (Winning) deriving (Eq, Show)

type Board = Array (Int, Int) Cell

data Game = Game { gameBoard :: Board
                 , gamePlayer :: Player
                 , gameState :: State
                 , shapeList :: [Player]
                 , gameSolution :: Board
                 , solutionShapes :: [Maybe Player]
                 } deriving (Eq, Show)

n :: Int
n = 5

shapeList1 = concat $ repeat $ [PlayerX,PlayerO, PlayerX,PlayerY, PlayerX,PlayerW,PlayerX]
screenWidth :: Int
screenWidth = 640

screenHeight :: Int
screenHeight = 640

cellWidth :: Float
cellWidth = fromIntegral screenWidth / fromIntegral n

cellHeight :: Float
cellHeight = fromIntegral screenHeight / fromIntegral n

smallPicWidth :: Float
smallPicWidth = 0.25*cellWidth

smallPicHeight :: Float
smallPicHeight = 0.25 * cellHeight

initialGame = Game { gameBoard = array indexRange $ zip (range indexRange) (repeat Nothing)
                   , gamePlayer = PlayerX
                   , gameState = Running
                   , shapeList = shapeList1
                   , gameSolution = (solution playerlist2)
                   , solutionShapes = playerlist2
                   }
    where indexRange = ((0, 0), (n - 1, n - 1))

transposed :: [(a,b)]->[(b,a)]
transposed tuplelist = map swap tuplelist

solution playerlist = array indexRange $ zip (transposed (range indexRange))(playerlist ++ repeat Nothing)
    where indexRange = ((0, 0), (n - 1, n - 1))

playerlist1=[Just PlayerX, Just PlayerX, Just PlayerO, Nothing, Nothing,Just PlayerX, Just PlayerX, Just PlayerW, Just PlayerO, Nothing,Just PlayerX, Just PlayerX, Just PlayerW, Just PlayerY, Nothing,Just PlayerX, Just PlayerX, Just PlayerY,Nothing,Nothing]

playerlist2 = [Just PlayerX, Just PlayerX, Just PlayerO, Nothing, Nothing,Just PlayerX, Just PlayerX, Just PlayerY, Nothing, Nothing, Just PlayerX, Just PlayerW, Just PlayerO, Nothing, Nothing, Just PlayerX, Just PlayerX, Just PlayerY, Nothing,Nothing]
