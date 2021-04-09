module Logic where

import Data.Array
import Data.Foldable ( asum )

import Game
import Graphics.Gloss.Interface.Pure.Game
import Data.Maybe (fromJust, isJust, isNothing)

isCoordCorrect = inRange ((0, 0), (n - 1, n - 2))

isSelectNewGame = (==) (2, n - 1)


switchShape' game = game {gameShape = head (shapeList game), shapeList = tail (shapeList game)}

full :: [Cell] -> Maybe Shape
full (cell@(Just shape):cells) | all (== cell) cells = Just shape
full _                                               = Nothing


winner2 :: Board -> Board -> Maybe (Winning)
winner2 board solution
  |board == solution = Just Winning
  |otherwise = Nothing


countCells :: Cell -> Board -> Int
countCells cell = length . filter ((==) cell) . elems

getCellShape game cellCoord = board ! cellCoord
  where board = gameBoard game

checkGameOver2 game 
    | Just Winning <- winner2 board solution =
        game { gameState = GameOver $ Winning }
    | countCells Nothing board <= 5 =
        game { gameState = GameOver Losing}
    | otherwise = game
    where board = gameBoard game
          solution = gameSolution game


playerTurn :: Game -> (Int, Int) -> Game
playerTurn game cellCoord
    | isCoordCorrect cellCoord && isNothing (board ! cellCoord) =
        checkGameOver2
        $ switchShape'
        $ game { gameBoard = board // [(cellCoord, Just shape)] }
    |isSelectNewGame cellCoord = initialGame (nextSolutions game) (nextRandomGen game)
    | otherwise = game
    where board = gameBoard game
          shape = gameShape game

removeShape :: Game -> (Int, Int) -> Game
removeShape game cellCoord
    | isCoordCorrect cellCoord && isJust removedShape  =
        checkGameOver2
        
        $ game { gameBoard = board // [(cellCoord, Nothing)],shapeList= (fromJust removedShape):currentShapes }
    | otherwise = game
    where board = gameBoard game
          removedShape=getCellShape game cellCoord
          currentShapes=shapeList game


turnShapeRight :: Game -> (Int, Int) -> Game
turnShapeRight game cellCoord
    | isCoordCorrect cellCoord && isJust turnedShape  =
        checkGameOver2
        
        $ game { gameBoard = board // [(cellCoord, turnRight turnedShape)] }
    | otherwise = game
    where board = gameBoard game
          turnedShape=getCellShape game cellCoord
          currentShapes=shapeList game

turnRight :: Cell -> Cell
turnRight (Just LTriangle)      = Just RTriangle
turnRight (Just RTriangle)      = Just LDownTriangle
turnRight (Just LDownTriangle)  = Just RDownTriangle
turnRight (Just RDownTriangle)  = Just LTriangle
turnRight (Just SemiCircle)     = Just SemiCircleL
turnRight (Just SemiCircleL)    = Just SemiCircleDown
turnRight (Just SemiCircleDown) = Just SemiCircleR
turnRight (Just SemiCircleR)    = Just SemiCircle
turnRight x = x

turnShapeLeft :: Game -> (Int, Int) -> Game
turnShapeLeft game cellCoord
    | isCoordCorrect cellCoord && isJust turnedShape  =
        checkGameOver2
        $ game { gameBoard = board // [(cellCoord, turnLeft turnedShape)] }
    | otherwise = game
    where board = gameBoard game
          turnedShape = getCellShape game cellCoord
          currentShapes = shapeList game
         
turnLeft :: Cell -> Cell
turnLeft (Just RTriangle)      = Just LTriangle
turnLeft (Just LTriangle)      = Just RDownTriangle
turnLeft (Just RDownTriangle)  = Just LDownTriangle
turnLeft (Just LDownTriangle)  = Just RTriangle
turnLeft (Just SemiCircle)     = Just SemiCircleR
turnLeft (Just SemiCircleR)    = Just SemiCircleDown
turnLeft (Just SemiCircleDown) = Just SemiCircleL
turnLeft (Just SemiCircleL)    = Just SemiCircle
turnLeft x                     = x




mousePosAsCellCoord :: (Float, Float) -> (Int, Int)
mousePosAsCellCoord (x, y) = ( floor ((y + (fromIntegral screenHeight * 0.5)) / cellHeight)
                             , floor ((x + (fromIntegral screenWidth * 0.5)) / cellWidth)
                             )

transformGame (EventKey (MouseButton LeftButton) Up _ mousePos) game =
    case gameState game of
      Running -> playerTurn game $ mousePosAsCellCoord mousePos
      GameOver _ -> initialGame (nextSolutions game) (nextRandomGen game)
      
transformGame (EventKey (MouseButton RightButton) Up _ mousePos) game =
  case gameState game of
    Running -> removeShape game $ mousePosAsCellCoord mousePos
    GameOver _ -> initialGame (nextSolutions game) (nextRandomGen game)

transformGame (EventKey (SpecialKey KeyLeft) Up _ mousePos) game =
    case gameState game of
      Running -> turnShapeLeft game $ mousePosAsCellCoord mousePos
      GameOver _ -> initialGame (nextSolutions game) (nextRandomGen game)

transformGame (EventKey (SpecialKey KeyRight) Up _ mousePos) game =
    case gameState game of
      Running -> turnShapeRight game $ mousePosAsCellCoord mousePos
      GameOver _ -> initialGame (nextSolutions game) (nextRandomGen game)
      
transformGame _ game = game


