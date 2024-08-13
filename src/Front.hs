module Front
    where

import Data.IORef
import Control.Monad
import Data.Strict.Tuple hiding (fst, snd) 

import           System.Console.Haskeline -- DEBUG

import qualified Graphics.UI.Threepenny      as UI
import           Graphics.UI.Threepenny.Canvas as Canvas
import           Graphics.UI.Threepenny.Core hiding (grid)
--import Distribution.Compat.Prelude
import qualified Data.Vector as V

import Common
import Automata
import Monads

-- startCA :: IO ()
-- startCA = startGUI defaultConfig { jsStatic = Just "."} setup --, jsLog = "Test" 


setupFront :: Window -> UI ()
setupFront window = void $ do
    UI.addStyleSheet window "foundation-5.css"
    UI.addStyleSheet window "grid.css"
    return window # set UI.title "Cellular Automata"


    (canvasContainer, canvas) <- drawCanvas (floor cellSize) (floor canvasSize)
    
    -- DEBUG
    (wrap, debugWrap, out, debug) <- debugUI canvas
    
    -- Buttons
    clear    <- clearButton canvas
    test     <- UI.button #+ [string "Test"]

    body <- UI.div #. "page-container" #+
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

    getBody window #+ [ pure body ]

    let
        clickClear :: Event Comm
        clickClear = const (UpdateCell (0,0) "dead") <$> UI.click clear

        clickCanvas :: Event Comm
        clickCanvas =   (\pos -> UpdateCell pos "black") <$>
                        (\pos -> getIndex canvas (fst pos) (snd pos) cellSize) <$>
                        UI.mousedown canvas

        interactions :: Event Comm
        interactions = UI.unionWith const clickClear clickCanvas

        commands :: Event (Either Error Env -> Either Error Env)    
        commands = fmap evalUp interactions

    calcBehaviour <- accumB (Right initEnv) commands
    -- calcBehaviour :: Behavior (Either Error Env)

    let res = fmap (\x -> case x of
                            Left err -> show err
                            Right env -> printGrid env) calcBehaviour
        errorB = fmap (\x -> case x of
                            Left err -> True
                            Right env -> False) calcBehaviour

    --element test # sink UI.enabled (not <$> errorB)

    element body # sink detectError errorB

    element debug # sink text res

    element canvas # sink updateCanvas calcBehaviour


updateCanvas :: WriteAttr Element (Either Error Env)
updateCanvas = mkWriteAttr $ \either canvas -> 
    case either of
        Left err -> return ()
        Right env -> do
            let gridD = fst env
                cells = snd env
                autGrid = grid gridD
            canvas # UI.clearCanvas
            forM_ [0..(height gridD)-1] $ \y -> do
                forM_ [0..(width gridD)-1] $ \x -> do
                    let cellId = (autGrid V.! y) V.! x
                        cell = searchCellId env cellId
                    case cell of
                        Nothing -> return ()
                        Just c -> do
                            let color = colour c
                            drawSquare canvas (fromIntegral x * cellSize) (fromIntegral y * cellSize) cellSize color
            return ()

detectError :: WriteAttr Element Bool
detectError = mkWriteAttr $ \error body -> do
    if error then
        do  element body # set style [("pointer-events","none")]
            element body # set style [("cursor", "not-allowed"), 
                                      ("border", "5px solid red")]
            return ()
    else
        return ()

-- drawnPoints :: WriteAttr Canvas (Either Error Env)
-- drawnPoints = mkWriteAttr $ \canvas calc -> do
--     UI.clearCanvas canvas
--     -- Draw the points here

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
                    # set style [("width","500px"),("height","100px"),("border","solid black 1px")]
                    # set (attr "tabindex") "1" -- allow key presses
                    #+ [element debug]
                return (wrap, debugWrap, out, debug)




drawSquare :: Canvas -> Double -> Double -> Double -> String -> UI()
drawSquare canvas x y size colour = 
    let newX =  fromIntegral(floor(x/cellSize)) * cellSize + 1
        newY =  fromIntegral(floor(y/cellSize)) * cellSize + 1
    in  do  canvas # set' UI.fillStyle (UI.htmlColor colour)
            canvas # UI.fillRect (newX,newY) (size-2) (size-2)

getIndex :: Canvas -> Double -> Double -> Double -> Pos
getIndex canvas x y size = 
    let newX =  floor(x/cellSize)
        newY =  floor(y/cellSize)
    in (newX, newY)

fromIntegralPoint :: (Int, Int) -> (Double, Double)
fromIntegralPoint (x, y) =  (fromIntegral x, fromIntegral y)

clearButton :: Element -> UI Element
clearButton canvas =    do  button <- UI.button #+ [string "Clear"]
                            --on UI.click button $ const $
                                --canvas # UI.clearCanvas DEPRECATED
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
