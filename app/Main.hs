module Main where

import           Graphics.Gloss
import           Graphics.Gloss.Data.Color

import           Game
import           Logic
import           Rendering
import           System.Random

window = InWindow "The building Game" (screenWidth, screenHeight) (100, 100)

backgroundColor = makeColor 0 0 0 255

main :: IO ()
main = do
  randn <- randomRIO (0, 100)
  print randn
  let gen = randomGenStd randn
  play
    window
    backgroundColor
    30
    (initialGame (solutionlistShuffled gen) gen)
    gameAsPicture
    transformGame
    (const id)
