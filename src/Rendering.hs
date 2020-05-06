module Rendering where

import Data.Array

import Graphics.Gloss

import Game

boardGridColor = makeColorI 255 255 255 255
squareColor = makeColorI 255 50 50 255
triangleColor = makeColorI 50 100 255 255
triangleRColor = makeColorI 50 255 50 255
freeFieldColor = makeColorI 255 255 255 255
losingColor = greyN 0.5

shapeColor Square = squareColor
shapeColor LTriangle = triangleColor
shapeColor RTriangle = triangleRColor
shapeColor Window = triangleRColor

boardAsRunningPicture game  =
    pictures [ color squareColor $ squareCellsOfBoard board
             , color triangleColor $ triangleCellsOfBoard board
             , color triangleRColor $ triangleRCellsOfBoard board
             , color triangleRColor $ windowCellsOfBoard board
             , color freeFieldColor $ freeFields freeFieldPicture
             , color (shapeColor player) $ indicationSquare (indicationPicture player)
             , color triangleColor $ indicationText textPicture
             , color triangleColor $ suggestedText textSuggested
             , color triangleColor $ pictures $ suggested (suggestedPicture solutionshapes) (suggestedpos 5 4)
             
             , color boardGridColor $ boardGrid
             ]
  where board = gameBoard game
        solutionshapes = solutionShapes game
        player = gameShape game

outcomeColor Winning = triangleColor
outcomeColor Losing = losingColor


snapPictureToCell picture (row, column) = translate x y picture
    where x = fromIntegral column * cellWidth + cellWidth * 0.5
          y = fromIntegral row * cellHeight + cellHeight * 0.5

indicationSquare picture = translate x y picture
  where x = fromIntegral 4 * cellWidth + cellWidth * 0.5
        y = fromIntegral 3 * cellHeight + cellHeight * 0.5

indicationText picture = translate x y picture
  where x = fromIntegral 4 * cellWidth 
        y = fromIntegral 4 * cellHeight + cellHeight * 0.5

suggested pictures xylist = zipWith (uncurry translate ) xylist pictures
      

suggestedText picture = translate x y picture
  where x = fromIntegral 4 * cellWidth 
        y = fromIntegral 2 * cellHeight + cellHeight * 0.5


freeFields picture = translate x y picture
  where x = fromIntegral 4 * cellWidth + cellWidth * 0.5
        y = fromIntegral 2 * cellHeight + cellHeight * 0.5

--indicationPicture :: Picture
indicationPicture Square= squareCell
 
indicationPicture LTriangle= triangleCellL
  
indicationPicture RTriangle= triangleCellR
 
indicationPicture Window= windowCell
  

suggestedPicture :: [Maybe Shape]->[Picture]
suggestedPicture shapelist=  map ($smallPicWidth) $ map gameShape2ShapeFunction shapelist


suggestedpos nrows ncols = [ (base + (0.5+c)*smallPicWidth ,(0.5+r)*smallPicHeight)| c<-[0..ncols-1],r<-[0..nrows-1]] 
  where base = fromIntegral 4 * cellWidth
        
freeFieldPicture :: Picture
freeFieldPicture = rectangleSolid (side) (5.0 * side)
  where side = min cellWidth cellHeight

textPicture :: Picture
textPicture = scale 0.18 0.18 $ text "next shape"

textSuggested :: Picture
textSuggested = scale 0.18 0.18 $ text "build this:"

textNext :: Picture
textNext = scale 0.18 0.18 $ text "Next game"


textGameOver ::Winning ->  Picture
textGameOver Winning = scale 0.18 0.18 $ text "You win!"
textGameOver Losing = scale 0.18 0.18 $ text "Game over"

squareCell :: Picture
squareCell = squarePic (cellHeight * 0.75)

windowCell :: Picture
windowCell = windowPic (cellHeight * 0.75)

triangleCellL :: Picture
triangleCellL = trianglePicL (cellHeight * 0.75)
        

triangleCellR :: Picture
triangleCellR =  trianglePicR (cellHeight * 0.75)
         
triangleCellM :: Picture
triangleCellM = polygon [(-halfside,-halfside),(halfside,-halfside),(0,halfside),(-halfside,-halfside)]
    where side = min cellWidth cellHeight * 0.75
          halfside = side * 0.5

trianglePicL :: Float -> Picture
trianglePicL  size = polygon [(-0.5 * size ,-0.5* size),(0.5 * size,-0.5 * size),(0.5 * size,0.5*size),(-0.5*size,-0.5*size)]

trianglePicR :: Float ->Picture
trianglePicR size = polygon [(-0.5*size,-0.5*size),(0.5 * size,-0.5*size),(-0.5*size,0.5*size),(-0.5*size,-0.5*size)]


squarePic :: Float -> Picture
squarePic size = rectangleSolid size size

windowPic :: Float ->Picture
windowPic size = rectangleWire size size
                 
noPic :: Float ->Picture
noPic _ = rectangleWire 0 0
 


cellsOfBoard :: Board -> Cell -> Picture -> Picture
cellsOfBoard board cell cellPicture =
    pictures
    $ map (snapPictureToCell cellPicture . fst)
    $ filter (\(_, e) -> e == cell)
    $ assocs board

indicatorCellOfBoad board cellPicture =
  undefined

squareCellsOfBoard :: Board -> Picture
squareCellsOfBoard board = cellsOfBoard board (Just Square) squareCell

windowCellsOfBoard :: Board -> Picture
windowCellsOfBoard board = cellsOfBoard board (Just Window) windowCell

triangleCellsOfBoard :: Board -> Picture
triangleCellsOfBoard board = cellsOfBoard board (Just LTriangle) triangleCellL

triangleRCellsOfBoard :: Board -> Picture
triangleRCellsOfBoard board = cellsOfBoard board (Just RTriangle ) triangleCellR

boardGrid :: Picture
boardGrid =
    pictures
    $ concatMap (\i -> [ line [ (i * cellWidth, 0.0)
                              , (i * cellWidth, fromIntegral screenHeight)
                              ]
                       , line [ (0.0,                      i * cellHeight)
                              , ((fromIntegral screenWidth)-cellWidth, i * cellHeight)
                              ]
                       ])
      [0.0 .. fromIntegral (n-1)]



gameShape2ShapeFunction (Just Square) = squarePic
gameShape2ShapeFunction (Just LTriangle) = trianglePicL
gameShape2ShapeFunction (Just RTriangle) = trianglePicR
gameShape2ShapeFunction (Just Window) = windowPic
gameShape2ShapeFunction Nothing = noPic

boardAsPicture game winning=
    pictures [ squareCellsOfBoard board
             , triangleCellsOfBoard board
             , triangleRCellsOfBoard board
             
             , windowCellsOfBoard board
             , color freeFieldColor $freeFields freeFieldPicture
             , indicationText (textGameOver winning)
              , color triangleColor $ suggestedText textNext
             , pictures $ suggested (suggestedPicture solutionshapes)  (suggestedpos 5 4)
             ,  color boardGridColor $boardGrid
             ]
  where board = gameBoard game
        solutionshapes = solutionShapes game
boardAsGameOverPicture winning game = color (outcomeColor winning) (boardAsPicture game winning)

gameAsPicture :: Game -> Picture
gameAsPicture game = translate (fromIntegral screenWidth * (-0.5))
                               (fromIntegral screenHeight * (-0.5))
                               frame
    where frame = case gameState game of
                    Running -> boardAsRunningPicture game 
                    GameOver winning -> boardAsGameOverPicture winning game
