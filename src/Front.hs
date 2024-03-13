module Front
    where

import Data.IORef
import Control.Monad
import Data.Strict.Tuple hiding (fst, snd) 

import           System.Console.Haskeline -- DEBUG

import           Graphics.UI.Threepenny      as UI
import           Graphics.UI.Threepenny.Canvas as Canvas
import           Graphics.UI.Threepenny.Core
--import Distribution.Compat.Prelude

import Common
import Automata
import Monads

cellSize = 25 :: Double
canvasSize = 500 :: Double

-- startCA :: IO ()
-- startCA = startGUI defaultConfig { jsStatic = Just "."} setup --, jsLog = "Test" 


setupFront :: Window -> UI ()
setupFront env window = do
    UI.addStyleSheet window "foundation-5.css"
    UI.addStyleSheet window "grid.css"
    return window # set UI.title "Cellular Automata"
    getBody window #+ [ automataDisplay window ]
    return ()


automataDisplay :: Window -> UI Element
automataDisplay window = do
    (canvasContainer, canvas) <- drawCanvas (floor cellSize) (floor canvasSize)
    
    -- DEBUG
    (wrap, debugWrap, out, debug) <- debugUI canvas
    
    -- Buttons
    clear    <- clearButton canvas
    test     <- UI.button #+ [string "Test"]

    env <- liftIO $ newIORef initEnv
    selectedCell <- liftIO $ newIORef initEnv
    --case runStateError (addCell var col xs ys) 


    on UI.mousedown canvas $ \(x, y) -> do
        posC <- getIndex canvas x y cellSize
        env <- liftIO $ readIORef env
        case runStateError (checkGrid posC) env of
            Left err -> throwError canvas canvasContainer
            Right (cellId :!: env) -> element debug # set text ("DEBUG: " ++ show cellId)
        drawSquare canvas x y cellSize "Red"

    UI.div #. "page-container" #+
        [
            UI.div #. "header"#+
                [element wrap, element debugWrap],
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


throwError :: Element -> Element -> UI Element
throwError canvas canvasCont = do   element canvas # set style [("pointer-events","none")]
                                    element canvasCont # set style [("cursor", "not-allowed"), 
                                                                    ("border", "2px solid red")]

                                               
                               



debugUI :: Element -> UI ( Element, Element, Element, Element)
debugUI canvas =  do   -- Mouse
                out  <- UI.span # set text "Coordinates: "
                wrap <- UI.div #. "wrap"
                    # set style [("width","300px"),("height","100px"),("border","solid black 1px")]
                    # set (attr "tabindex") "1" -- allow key presses
                    #+ [element out]

                    -- Mouse detection
                on UI.mousemove canvas $ \xy ->
                    element out # set text ("Coordinates: " ++ show xy)

                    -- DEBUG
                debug  <- UI.span # set text "DEBUG: "
                debugWrap <- UI.div #. "wrap"
                    # set style [("width","300px"),("height","100px"),("border","solid black 1px")]
                    # set (attr "tabindex") "1" -- allow key presses
                    #+ [element debug]
                return (wrap, debugWrap, out, debug)




drawSquare :: Canvas -> Double -> Double -> Double -> String -> UI()
drawSquare canvas x y size colour = 
    let newX =  fromIntegral(floor(x/cellSize)) * cellSize + 1
        newY =  fromIntegral(floor(y/cellSize)) * cellSize + 1
    in  do  canvas # set' UI.fillStyle (UI.htmlColor colour)
            canvas # UI.fillRect (newX,newY) (size-2) (size-2)

getIndex :: Canvas -> Double -> Double -> Double -> UI Pos
getIndex canvas x y size = 
    let newX =  floor(x/cellSize)
        newY =  floor(y/cellSize)
    in  return (newX, newY)

fromIntegralPoint :: (Int, Int) -> (Double, Double)
fromIntegralPoint (x, y) =  (fromIntegral x, fromIntegral y)

clearButton :: Element -> UI Element
clearButton canvas =    do  button <- UI.button #+ [string "Clear"]
                            on UI.click button $ const $
                                canvas # UI.clearCanvas
                            return button

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

getElem :: Window -> String -> UI Element
getElem window str = do     elemList <- getElementsByTagName window str
                            return (head elemList)
