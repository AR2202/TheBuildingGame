module Rendering where

import Data.Array

import Graphics.Gloss

import Game

boardGridColor = makeColorI 255 255 255 255
squareColor = makeColorI 255 50 50 255
triangleColor = makeColorI 50 100 255 255
triangleRColor = makeColorI 50 255 50 255
freeFieldColor = makeColorI 255 255 255 255
tieColor = greyN 0.5

shapeColor PlayerX = squareColor
shapeColor PlayerO = triangleColor
shapeColor PlayerY = triangleRColor
shapeColor PlayerW = triangleRColor

boardAsRunningPicture board player =
    pictures [ color squareColor $ squareCellsOfBoard board
             , color triangleColor $ triangleCellsOfBoard board
             , color triangleRColor $ triangleRCellsOfBoard board
             , color triangleRColor $ windowCellsOfBoard board
             , color freeFieldColor $ freeFields freeFieldPicture
             , color (shapeColor player) $ indicationSquare (indicationPicture player)
             , color triangleColor $ indicationText textPicture
             , color triangleColor $ suggestedText textSuggested
             , color triangleColor $ pictures $ suggested (suggestedPicture playerlist2) (suggestedpos 5 4)
             
             , color boardGridColor $ boardGrid
             ]

outcomeColor Winning = triangleColor
outcomeColor Losing = tieColor


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
indicationPicture PlayerX= squareCell
 
indicationPicture PlayerO= triangleCellL
  
indicationPicture PlayerY= triangleCellR
 
indicationPicture PlayerW= windowCell
  

suggestedPicture :: [Maybe Player]->[Picture]
suggestedPicture playerlist=  map ($smallPicWidth) $ map gamePlayer2ShapeFunction playerlist


suggestedpos nrows ncols = [ (base + (0.5+c)*smallPicWidth ,(0.5+r)*smallPicHeight)| c<-[0..ncols-1],r<-[0..nrows-1]] 
  where base = fromIntegral 4 * cellWidth
        
freeFieldPicture :: Picture
freeFieldPicture = rectangleSolid (side) (5.0 * side)
  where side = min cellWidth cellHeight

textPicture :: Picture
textPicture = scale 0.18 0.18 $ text "next shape"

textSuggested :: Picture
textSuggested = scale 0.18 0.18 $ text "build this:"


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
squareCellsOfBoard board = cellsOfBoard board (Just PlayerX) squareCell

windowCellsOfBoard :: Board -> Picture
windowCellsOfBoard board = cellsOfBoard board (Just PlayerW) windowCell

triangleCellsOfBoard :: Board -> Picture
triangleCellsOfBoard board = cellsOfBoard board (Just PlayerO) triangleCellL

triangleRCellsOfBoard :: Board -> Picture
triangleRCellsOfBoard board = cellsOfBoard board (Just PlayerY) triangleCellR

boardGrid :: Picture
boardGrid =
    pictures
    $ concatMap (\i -> [ line [ (i * cellWidth, 0.0)
                              , (i * cellWidth, fromIntegral screenHeight)
                              ]
                       , line [ (0.0,                      i * cellHeight)
                              , (fromIntegral screenWidth, i * cellHeight)
                              ]
                       ])
      [0.0 .. fromIntegral n]


gamePlayer2ShapeFunction (Just PlayerX) = squarePic
gamePlayer2ShapeFunction (Just PlayerO) = trianglePicL
gamePlayer2ShapeFunction (Just PlayerY) = trianglePicR
gamePlayer2ShapeFunction (Just PlayerW) = windowPic
gamePlayer2ShapeFunction Nothing = noPic

boardAsPicture board winning=
    pictures [ squareCellsOfBoard board
             , triangleCellsOfBoard board
             , triangleRCellsOfBoard board
             
             , windowCellsOfBoard board
             , color freeFieldColor $freeFields freeFieldPicture
             , indicationText (textGameOver winning)
             , pictures $ suggested (suggestedPicture playerlist2)  (suggestedpos 5 4)
             ,  color boardGridColor $boardGrid
             ]

boardAsGameOverPicture winning board = color (outcomeColor winning) (boardAsPicture board winning)

gameAsPicture :: Game -> Picture
gameAsPicture game = translate (fromIntegral screenWidth * (-0.5))
                               (fromIntegral screenHeight * (-0.5))
                               frame
    where frame = case gameState game of
                    Running -> boardAsRunningPicture (gameBoard game) (gamePlayer game)
                    GameOver winner -> boardAsGameOverPicture winner (gameBoard game) 
