module Lib
    ( startCA
    ) where

import Data.IORef
import Control.Monad


import           Graphics.UI.Threepenny      as UI
import           Graphics.UI.Threepenny.Canvas as Canvas
import           Graphics.UI.Threepenny.Core
--import Distribution.Compat.Prelude

cellSize = 25 
canvasSize = 700 
padding = 10  --padding

startCA :: IO ()
startCA = startGUI defaultConfig { jsStatic = Just "." } setup

drawSquare :: Canvas -> Double -> Double -> Double -> String -> UI()
drawSquare canvas x y size colour = do  canvas # set' UI.fillStyle (UI.htmlColor colour)
                                        canvas # UI.fillRect (x,y) size size


fromIntegralPoint :: (Int, Int) -> (Double, Double)
fromIntegralPoint (x, y) =  (fromIntegral x, fromIntegral y)

setup :: Window -> UI ()
setup window = do
    UI.addStyleSheet window "foundation-5.css"
    UI.addStyleSheet window "grid.css"
    return  window # set UI.title "Cellular Automata"


    -- Canvas --
    canvas <- UI.canvas #. "canvas"
        # set UI.height canvasSize
        # set UI.width  canvasSize

    -- Draw grid
    forM_ [0,cellSize..canvasSize] $ \x -> do
        UI.moveTo (fromIntegralPoint (x + padding + 1, padding)) canvas
        UI.lineTo (fromIntegralPoint (x + padding + 1, canvasSize + padding)) canvas
        
        UI.moveTo (fromIntegralPoint (padding, x + padding + 1)) canvas
        UI.lineTo (fromIntegralPoint (canvasSize + padding, x + padding + 1)) canvas

    -- Buttons
    clear     <- UI.button #+ [string "Clear"]
    drawRects <- UI.button #+ [string "Add some rectangles."]

    -- Button Actions
    on UI.click clear $ const $
        canvas # UI.clearCanvas

    -- Mouse
    out  <- UI.span # set text "Coordinates: "
    wrap <- UI.div #. "wrap"
        # set style [("width","300px"),("height","300px"),("border","solid black 1px")]
        # set (attr "tabindex") "1" -- allow key presses
        #+ [element out]

    -- Mouse detection
    on UI.mousemove canvas $ \xy ->
        element out # set text ("Coordinates: " ++ show xy)
    on UI.mousedown canvas $ \(x, y) ->
        drawSquare canvas x y 25 "Red"

    getBody window #+ 
        [
            UI.div #. "page-container" #+
                [
                    UI.div #. "header"#+
                        [element wrap],
                    UI.div #. "menu"#+
                        [element clear, 
                        element drawRects],
                    UI.div #. "main"#+
                        [element canvas],
                    UI.div #. "right",
                    UI.div #. "footer"

                ]
        ]
    
    -- draw some rectangles
    on UI.click drawRects $ const $ do
        let rects = [ (20 , 130, 15, 120, "teal")
                    , (345, 110, 15, 90, "lightblue")
                    , (220, 360, 95, 15, "teal")
                    ]
        forM_ rects $ \(x,y,w,h,color) -> do
            canvas # set' UI.fillStyle (UI.htmlColor color)
            canvas # UI.fillRect (x,y) w h

    return()
         
