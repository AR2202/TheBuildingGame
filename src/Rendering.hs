module Rendering where

import           Data.Array

import           Graphics.Gloss

import           Game

boardGridColor = makeColorI 255 255 255 255

squareColor = makeColorI 255 50 50 255

triangleColor = makeColorI 50 100 255 255

circleColor = makeColorI 100 255 255 100

semiCircleColor = makeColorI 255 255 0 255

triangleRColor = makeColorI 50 255 50 255

freeFieldColor = makeColorI 255 255 255 255

losingColor = greyN 0.5

shapeColor Square         = squareColor
shapeColor RDownTriangle  = triangleColor
shapeColor Window         = triangleRColor
shapeColor CircleS        = circleColor
shapeColor SemiCircle     = semiCircleColor
shapeColor SemiCircleR    = semiCircleColor
shapeColor SemiCircleL    = semiCircleColor
shapeColor SemiCircleDown = semiCircleColor
shapeColor _              = triangleColor

boardAsRunningPicture game =
  pictures $
  [ color freeFieldColor $ freeFields freeFieldPicture
  , color triangleColor $
    pictures $ suggested (suggestedPicture solutionshapes) (suggestedpos 5 4)
  , color boardGridColor $ boardGrid
  , color (shapeColor shape) $ indicationSquare (indicationPicture shape)
  ] ++
  map
    (boardPictures board)
    [ Right Square
    , Right LTriangle
    , Right RDownTriangle
    , Right LDownTriangle
    , Right RTriangle
    , Right Window
    , Right CircleS
    , Right SemiCircle
    , Right SemiCircleR
    , Right SemiCircleL
    , Right SemiCircleDown
    , Left (nextTexty, "next game")
    , Left (turn1Texty, "use arrow keys")
    , Left (turn2Texty, "to turn")
    , Left (quitTexty, "quit: esc")
    , Left (indicationTexty, "next shape")
    , Left (suggestedTexty, "build this")
    ]
  where
    board = gameBoard game
    solutionshapes = solutionShapes game
    shape = gameShape game

outcomeColor Winning = triangleColor
outcomeColor Losing  = losingColor

boardPictures :: Board -> Either (Float, String) Shape -> Picture
boardPictures _ (Left (ypos, string)) =
  color triangleColor $ positionText string ypos
boardPictures board (Right shape) =
  color (shapeColor shape) $ picCellOfBoard board shape

snapPictureToCell picture (row, column) = translate x y picture
  where
    x = fromIntegral column * cellWidth + cellWidth * 0.5
    y = fromIntegral row * cellHeight + cellHeight * 0.5

positionText text cellsy = translate x y $ textpicture
  where
    x = fromIntegral 4 * cellWidth
    y = cellsy * cellHeight
    textpicture = textPic text

indicationSquare picture = translate x y picture
  where
    x = fromIntegral 4 * cellWidth + cellWidth * 0.5
    y = fromIntegral 3 * cellHeight + cellHeight * 0.5

quitTexty = 4.5

suggestedTexty = 1.5

nextTexty = 2.25

turn1Texty = 2.8

turn2Texty = 2.6

indicationTexty = 4.0

suggested pictures xylist = zipWith (uncurry translate) xylist pictures

suggestedText picture = translate x y picture
  where
    x = fromIntegral 4 * cellWidth
    y = fromIntegral 1 * cellHeight + cellHeight * 0.5

freeFields picture = translate x y picture
  where
    x = fromIntegral 4 * cellWidth + cellWidth * 0.5
    y = fromIntegral 2 * cellHeight + cellHeight * 0.5

indicationPicture Square         = squarePic picSize
indicationPicture LTriangle      = trianglePicL picSize
indicationPicture RTriangle      = trianglePicR picSize
indicationPicture RDownTriangle  = trianglePicDownR picSize
indicationPicture LDownTriangle  = trianglePicDownL picSize
indicationPicture Window         = windowPic picSize
indicationPicture CircleS        = circlePic picSize
indicationPicture SemiCircle     = semiCirclePic picSize
indicationPicture SemiCircleR    = semiCirclePicR picSize
indicationPicture SemiCircleL    = semiCirclePicL picSize
indicationPicture SemiCircleDown = semiCirclePicDown picSize

suggestedPicture :: [Maybe Shape] -> [Picture]
suggestedPicture shapelist =
  map ($smallPicWidth) $ map gameShape2ShapeFunction shapelist

suggestedpos nrows ncols =
  [ (base + (0.5 + c) * smallPicWidth, (0.5 + r) * smallPicHeight)
  | c <- [0 .. ncols - 1]
  , r <- [0 .. nrows - 1]
  ]
  where
    base = fromIntegral 4 * cellWidth

freeFieldPicture :: Picture
freeFieldPicture = rectangleSolid (side) (5.0 * side)
  where
    side = min cellWidth cellHeight

textPic :: String -> Picture
textPic string = scale 0.18 0.18 $ text string

textNext :: Picture
textNext = textPic "Next game"

textGameOver :: Winning -> Picture
textGameOver Winning = textPic "You win!"
textGameOver Losing  = textPic "Game over"

picSize :: Float
picSize = cellHeight * 0.75

trianglePicL :: Float -> Picture
trianglePicL size =
  polygon
    [ (-0.5 * size, -0.5 * size)
    , (0.5 * size, -0.5 * size)
    , (0.5 * size, 0.5 * size)
    , (-0.5 * size, -0.5 * size)
    ]

trianglePicR :: Float -> Picture
trianglePicR size =
  polygon
    [ (-0.5 * size, -0.5 * size)
    , (0.5 * size, -0.5 * size)
    , (-0.5 * size, 0.5 * size)
    , (-0.5 * size, -0.5 * size)
    ]

trianglePicDownL :: Float -> Picture
trianglePicDownL size =
  polygon
    [ (-0.5 * size, 0.5 * size)
    , (0.5 * size, 0.5 * size)
    , (-0.5 * size, -0.5 * size)
    , (-0.5 * size, 0.5 * size)
    ]

trianglePicDownR :: Float -> Picture
trianglePicDownR size =
  polygon
    [ (-0.5 * size, 0.5 * size)
    , (0.5 * size, 0.5 * size)
    , (0.5 * size, -0.5 * size)
    , (-0.5 * size, 0.5 * size)
    ]

squarePic :: Float -> Picture
squarePic size = rectangleSolid size size

windowPic :: Float -> Picture
windowPic size = rectangleWire size size

circlePic :: Float -> Picture
circlePic size = circle (0.5 * size)

semiCirclePic :: Float -> Picture
semiCirclePic size = arc 0 180 (0.5 * size)

semiCirclePicR :: Float -> Picture
semiCirclePicR size = arc 90 270 (0.5 * size)

semiCirclePicL :: Float -> Picture
semiCirclePicL size = arc 270 90 (0.5 * size)

semiCirclePicDown :: Float -> Picture
semiCirclePicDown size = arc 180 360 (0.5 * size)

noPic :: Float -> Picture
noPic _ = rectangleWire 0 0

cellsOfBoard :: Board -> Cell -> Picture -> Picture
cellsOfBoard board cell cellPicture =
  pictures $
  map (snapPictureToCell cellPicture . fst) $
  filter (\(_, e) -> e == cell) $ assocs board

picCellOfBoard :: Board -> Shape -> Picture
picCellOfBoard board shape =
  cellsOfBoard board (Just shape) (indicationPicture shape)

squareCellsOfBoard :: Board -> Picture
squareCellsOfBoard board = picCellOfBoard board Square

windowCellsOfBoard :: Board -> Picture
windowCellsOfBoard board = picCellOfBoard board Window

triangleCellsOfBoard :: Board -> Picture
triangleCellsOfBoard board = picCellOfBoard board LTriangle

triangleRCellsOfBoard :: Board -> Picture
triangleRCellsOfBoard board = picCellOfBoard board RTriangle

triangleRDownCellsOfBoard :: Board -> Picture
triangleRDownCellsOfBoard board = picCellOfBoard board RDownTriangle

triangleLDownCellsOfBoard :: Board -> Picture
triangleLDownCellsOfBoard board = picCellOfBoard board LDownTriangle

circleCellsOfBoard :: Board -> Picture
circleCellsOfBoard board = picCellOfBoard board CircleS

semiCircleCellsOfBoard :: Board -> Picture
semiCircleCellsOfBoard board = picCellOfBoard board SemiCircle

semiCircleCellsROfBoard :: Board -> Picture
semiCircleCellsROfBoard board = picCellOfBoard board SemiCircleR

semiCircleCellsLOfBoard :: Board -> Picture
semiCircleCellsLOfBoard board = picCellOfBoard board SemiCircleL

semiCircleCellsDownOfBoard :: Board -> Picture
semiCircleCellsDownOfBoard board = picCellOfBoard board SemiCircleDown

boardGrid :: Picture
boardGrid =
  pictures $
  concatMap
    (\i ->
       [ line [(i * cellWidth, 0.0), (i * cellWidth, fromIntegral screenHeight)]
       , line
           [ (0.0, i * cellHeight)
           , ((fromIntegral screenWidth) - cellWidth, i * cellHeight)
           ]
       ])
    [0.0 .. fromIntegral (n - 1)]

gameShape2ShapeFunction (Just Square)         = squarePic
gameShape2ShapeFunction (Just LTriangle)      = trianglePicL
gameShape2ShapeFunction (Just RTriangle)      = trianglePicR
gameShape2ShapeFunction (Just LDownTriangle)  = trianglePicDownL
gameShape2ShapeFunction (Just Window)         = windowPic
gameShape2ShapeFunction (Just CircleS)        = circlePic
gameShape2ShapeFunction (Just SemiCircle)     = semiCirclePic
gameShape2ShapeFunction (Just SemiCircleR)    = semiCirclePicR
gameShape2ShapeFunction (Just SemiCircleL)    = semiCirclePicL
gameShape2ShapeFunction (Just SemiCircleDown) = semiCirclePicDown
gameShape2ShapeFunction Nothing               = noPic

boardAsPicture game winning =
  pictures
    [ squareCellsOfBoard board
    , triangleCellsOfBoard board
    , triangleRCellsOfBoard board
    , triangleRDownCellsOfBoard board
    , triangleLDownCellsOfBoard board
    , windowCellsOfBoard board
    , circleCellsOfBoard board
    , semiCircleCellsOfBoard board
    , semiCircleCellsROfBoard board
    , semiCircleCellsLOfBoard board
    , semiCircleCellsDownOfBoard board
    , color freeFieldColor $freeFields freeFieldPicture
    , positionText "you win!" indicationTexty
    , color triangleColor $ suggestedText textNext
    , pictures $ suggested (suggestedPicture solutionshapes) (suggestedpos 5 4)
    , color boardGridColor $boardGrid
    ]
  where
    board = gameBoard game
    solutionshapes = solutionShapes game

boardAsGameOverPicture winning game =
  color (outcomeColor winning) (boardAsPicture game winning)

gameAsPicture :: Game -> Picture
gameAsPicture game =
  translate
    (fromIntegral screenWidth * (-0.5))
    (fromIntegral screenHeight * (-0.5))
    frame
  where
    frame =
      case gameState game of
        Running          -> boardAsRunningPicture game
        GameOver winning -> boardAsGameOverPicture winning game
