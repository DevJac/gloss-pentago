import Graphics.Gloss (play, Display (InWindow))
import Graphics.Gloss.Data.Color (Color, makeColor, white, black)
import Graphics.Gloss.Data.Picture
    (Picture, pictures, blank, translate, color, thickCircle, rectangleSolid)
import Graphics.Gloss.Interface.Pure.Game
    (Event (EventKey), Key (MouseButton), MouseButton (LeftButton),
     KeyState (Up))
import qualified Data.Map as M
import Data.List (minimumBy)
import Data.Ord (comparing)
import Data.Maybe (isNothing)
import Debug.Trace

data World = World Bool Board

type Board = M.Map (Int, Int) Marker

data Marker = White | Black deriving Eq

instance Show Marker where
    show White = "W"
    show Black = "B"

backgroundColor :: Color
backgroundColor = makeColor 0.5 0.5 0.5 1

mainBoardColor :: Color
mainBoardColor = makeColor 0.7 0 0 1

secondaryBoardColor :: Color
secondaryBoardColor = makeColor 0.5 0 0 1

main :: IO ()
main = play
    (InWindow "Pentago" (600, 600) (100, 100))
    backgroundColor
    20
    (World False M.empty)
    render
    handleInput
    step

renderQuadrant :: Picture
renderQuadrant = pictures $
    [ color mainBoardColor $ rectangleSolid 140 140 ] ++
    [ color secondaryBoardColor $ translate x y $ thickCircle 5 10
        | x <- [-50, 0, 50], y <- [-50, 0, 50] ]

renderBackground :: Picture
renderBackground = pictures
    [ translate x y $ renderQuadrant | x <- [-75, 75], y <- [-75, 75] ]

renderMarkers :: Board -> Picture
renderMarkers b =
    pictures [ case M.lookup (x, y) b of
            Just White -> translate (t $ fromIntegral x) (t $ fromIntegral y) .
                          color white $
                          thickCircle 5 20
            Just Black -> translate (t $ fromIntegral x) (t $ fromIntegral y) .
                          color black $
                          thickCircle 5 20
            _          -> blank
        | x <- [0..5], y <- [0..5] ]
    where t n = 125 - (50 * n)

render :: World -> Picture
render (World _ b) = pictures [ renderBackground, renderMarkers b ]

positionCoordinates :: [(Float, Float)]
positionCoordinates = [ (x, y) | x <- a, y <- a ]
                      where a = take 6 $ iterate (+50) (-125)

snap :: [(Float, Float)] -> (Float, Float) -> (Float, Float)
snap ts (x, y) = fst . minimumBy (comparing snd) $
                 map (\p@(a, b) -> (p, sqrt (abs(x-a)**2 + abs(y-b)**2))) ts

emptyPositions :: Board -> [(Int, Int)]
emptyPositions b = filter (\k -> isNothing $ M.lookup k b)
                   [ (x, y) | x <- [0..5], y <- [0..5] ]

currentPlayer :: Board -> Marker
currentPlayer b | even . length $ emptyPositions b = White
                | otherwise                        = Black

handleInput :: Event -> World -> World
handleInput (EventKey (MouseButton LeftButton) Up _ c) w@(World False b) =
    let (x, y) = snap positionCoordinates c
        p = ((125 - round x) `div` 50, (125 - round y) `div` 50)
    in if p `elem` emptyPositions b
       then World True (M.insert p (currentPlayer b) b)
       else w
handleInput _ w = w

step :: Float -> World -> World
step _ w = w
