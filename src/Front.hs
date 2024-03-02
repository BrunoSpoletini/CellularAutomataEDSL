module Front
    ( startCA
    ) where

import Data.IORef
import Control.Monad


import           Graphics.UI.Threepenny      as UI
import           Graphics.UI.Threepenny.Canvas as Canvas
import           Graphics.UI.Threepenny.Core
--import Distribution.Compat.Prelude

cellSize = 25 :: Double
canvasSize = 500 :: Double


startCA :: IO ()
startCA = startGUI defaultConfig { jsStatic = Just "." } setup

drawSquare :: Canvas -> Double -> Double -> Double -> String -> UI()
drawSquare canvas x y size colour = 
    let newX =  fromIntegral(floor(x/cellSize)) * cellSize + 1
        newY =  fromIntegral(floor(y/cellSize)) * cellSize + 1
    in  do  canvas # set' UI.fillStyle (UI.htmlColor colour)
            canvas # UI.fillRect (newX,newY) (size-2) (size-2)


fromIntegralPoint :: (Int, Int) -> (Double, Double)
fromIntegralPoint (x, y) =  (fromIntegral x, fromIntegral y)

setup :: Window -> UI ()
setup window = do
    UI.addStyleSheet window "foundation-5.css"
    UI.addStyleSheet window "grid.css"
    return  window # set UI.title "Cellular Automata"

    (canvasContainer, canvas) <- drawCanvas (floor cellSize) (floor canvasSize)

    -- Buttons
    clear     <- UI.button #+ [string "Clear"]
    test     <- UI.button #+ [string "Test"]

    -- Button Actions
    on UI.click clear $ const $
        canvas # UI.clearCanvas

    -- Mouse
    out  <- UI.span # set text "Coordinates: "
    wrap <- UI.div #. "wrap"
        # set style [("width","300px"),("height","100px"),("border","solid black 1px")]
        # set (attr "tabindex") "1" -- allow key presses
        #+ [element out]

    -- Mouse detection
    on UI.mousemove canvas $ \xy ->
        element out # set text ("Coordinates: " ++ show xy)
    on UI.mousedown canvas $ \(x, y) -> do
        drawSquare canvas x y cellSize "Red"
        -- --TEST VELOCIDAD
        -- forM_ [0,cellSize..canvasSize] $ \x -> do
        --     forM_ [0,cellSize..canvasSize] $ \y -> do
        --         drawSquare canvas x y cellSize "blue"



    getBody window #+ 
        [
            UI.div #. "page-container" #+
                [
                    UI.div #. "header"#+
                        [element wrap],
                    UI.div #. "menu"#+
                        --[
                            --UI.div #. "row"#+
                                [element clear, element test],
                        --],
                    UI.div #. "main"#+
                        [element canvasContainer],
                    UI.div #. "right",
                    UI.div #. "footer"
                ]
        ]
    

    return()
         

-- Draw the whole canvas
drawCanvas :: Int -> Int -> UI (Element, Element)
drawCanvas cellSize canvasSize = do
     -- Static Canvas --
    canvasBase <- UI.canvas #. "static-canvas"
        # set UI.height canvasSize
        # set UI.width  canvasSize
        # set UI.strokeStyle "black"

    -- Draw grid on Static Canvas
    forM_ [0,cellSize..canvasSize] $ \x -> do
        UI.moveTo (fromIntegralPoint (x, 0)) canvasBase
        UI.lineTo (fromIntegralPoint (x, canvasSize)) canvasBase
        
        UI.moveTo (fromIntegralPoint (0, x)) canvasBase
        UI.lineTo (fromIntegralPoint (canvasSize, x)) canvasBase
        UI.stroke canvasBase

    -- Volatile Canvas
    canvas <- UI.canvas #. "canvas"
        # set UI.height canvasSize
        # set UI.width  canvasSize

    -- Canvas container
    canvasContainer <- UI.div #. "canvas-container"
        # set style [("height", show (canvasSize+1) ++ "px"), ("width", show (canvasSize+1) ++ "px")]
        # set UI.draggable False
        #+ [element canvas, element canvasBase]
    return (canvasContainer, canvas)