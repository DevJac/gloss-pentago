import Graphics.Gloss (play, Display (InWindow))
import Graphics.Gloss.Data.Color (Color, makeColor)
import Graphics.Gloss.Data.Picture (Picture, blank)
import Graphics.Gloss.Interface.Pure.Game (Event)
import Data.Map as M

type Board = M.Map (Int, Int) Marker

data Marker = White | Black deriving Eq

instance Show Marker where
    show White = "W"
    show Black = "B"

backgroundColor :: Color
backgroundColor = makeColor 0.5 0.5 0.5 0

main :: IO ()
main = play
    (InWindow "Pentago" (600, 600) (100, 100))
    backgroundColor
    20
    M.empty
    render
    handleInput
    step

render :: Board -> Picture
render _ = blank

handleInput :: Event -> Board -> Board
handleInput _ b = b

step :: Float -> Board -> Board
step _ b = b
