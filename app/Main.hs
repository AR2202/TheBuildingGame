module Main where

import Graphics.Gloss
import Graphics.Gloss.Data.Color

import Game
import Logic
import Rendering

window = InWindow "The building Game" (screenWidth, screenHeight) (100, 100)
backgroundColor = makeColor 0 0 0 255

main :: IO ()
main = play window backgroundColor 30 (initialGame solutionlist randomGenStd) gameAsPicture transformGame (const id)


