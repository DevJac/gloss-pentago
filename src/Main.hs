import Graphics.Gloss (play, Display (InWindow))
import Graphics.Gloss.Data.Color (Color, makeColor, white, black)
import Graphics.Gloss.Data.Picture
    (Picture, pictures, blank, translate, color, thickCircle, rectangleSolid)
import Graphics.Gloss.Interface.Pure.Game (Event)
import qualified Data.Map as M
import Data.List (maximumBy)
import Data.Ord (comparing)

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
    M.empty
    render
    handleInput
    step

renderQuadrant :: Picture
renderQuadrant = pictures $
    [ color mainBoardColor $ rectangleSolid 140 140 ] ++
    [ color secondaryBoardColor $ translate x y $ thickCircle 5 10
        | x <- [-50, 0, 50]
        , y <- [-50, 0, 50] ]

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
    where t n = 125 + (50 * n)

render :: Board -> Picture
render b = pictures [ renderBackground, renderMarkers b ]

snap :: [(Float, Float)] -> (Float, Float) -> (Float, Float)
snap ts (x, y) = fst . maximumBy (comparing snd) $
                 map (\p@(a, b) -> (p, sqrt (abs(x-a)**2 + abs(y-b)**2))) ts

handleInput :: Event -> Board -> Board
handleInput _ b = b

step :: Float -> Board -> Board
step _ b = b
