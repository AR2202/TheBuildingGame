module Logic where

import Data.Array
import Data.Foldable ( asum )

import Game
import Graphics.Gloss.Interface.Pure.Game

isCoordCorrect = inRange ((0, 0), (n - 1, n - 2))



switchShape' game = game {gameShape = head (shapeList game), shapeList = tail (shapeList game)}

full :: [Cell] -> Maybe Shape
full (cell@(Just player):cells) | all (== cell) cells = Just player
full _                                                = Nothing


winner2 :: Board -> Board -> Maybe (Winning)
winner2 board solution
  |board == solution = Just Winning
  |otherwise = Nothing


countCells :: Cell -> Board -> Int
countCells cell = length . filter ((==) cell) . elems



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
    | isCoordCorrect cellCoord && board ! cellCoord == Nothing =
        checkGameOver2
        $ switchShape'
        $ game { gameBoard = board // [(cellCoord, Just player)] }
    | otherwise = game
    where board = gameBoard game
          player = gameShape game

removeShape :: Game -> (Int, Int) -> Game
removeShape game cellCoord
    | isCoordCorrect cellCoord  =
        checkGameOver2
        $ game { gameBoard = board // [(cellCoord, Nothing)] }
    | otherwise = game
    where board = gameBoard game
         

mousePosAsCellCoord :: (Float, Float) -> (Int, Int)
mousePosAsCellCoord (x, y) = ( floor ((y + (fromIntegral screenHeight * 0.5)) / cellHeight)
                             , floor ((x + (fromIntegral screenWidth * 0.5)) / cellWidth)
                             )

transformGame (EventKey (MouseButton LeftButton) Up _ mousePos) game =
    case gameState game of
      Running -> playerTurn game $ mousePosAsCellCoord mousePos
      GameOver _ -> initialGame (nextSolutions game)
      
transformGame (EventKey (MouseButton RightButton) Up _ mousePos) game =
  case gameState game of
    Running -> removeShape game $ mousePosAsCellCoord mousePos
    GameOver _ -> initialGame (nextSolutions game)
transformGame _ game = game


