{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
module Front
    where

import Data.Char (toUpper, toLower)
import Data.IORef
import Control.Monad
import Data.Strict.Tuple hiding (fst, snd, zip)

--import           System.Console.Haskeline -- DEBUG

import qualified Graphics.UI.Threepenny      as UI
import           Graphics.UI.Threepenny.Canvas as Canvas
import           Graphics.UI.Threepenny.Core hiding (grid)
--import Distribution.Compat.Prelude
import qualified Data.Vector as V

import Debug.Trace -- TO DO remove

import Common
import Automata
import Monads

setupFront :: Window -> Env -> UI ()
setupFront window fileEnv = void $ do
    UI.addStyleSheet window "grid.css"
    UI.addStyleSheet window "semantic.min.css"
    pure window # set UI.title "Cellular Automata"

    -- Header
    header <- UI.div #. "header"
                     # set text "Cellular Automata"

    -- Canvas
    (canvasContainer, canvas) <- drawCanvas (floor cellSize) (floor canvasSize)
    
    -- Selector de celulas
    cellButtPairL <- drawCellList (fst $ snd fileEnv) (snd $ snd fileEnv)
    cellSel <- UI.div #. "ui vertical menu"
                      #+ (  map element $ map fst cellButtPairL )

    -- Console
    console <- UI.div #. "ui segment console"
        # set (attr "contenteditable") "false"
        # set text "Historial de comandos"

    -- Timer
    timer <- UI.timer
            # set UI.interval 500
            # set UI.running False

    -- Botones de control
    (playContainer, reset) <- timeController timer console cellButtPairL

    url <- UI.loadFile "image/png" "static/triangulos.png"
    body <- UI.div #. "page-container" 
                   # set UI.style [("background-image", "url(" ++ url ++ ")"), ("background-size", "cover")]
                #+ [
                    element header,
                    UI.div #. "displayRow" #+ 
                    [
                        UI.div #. "menu"
                           #+ [element playContainer, element reset],
                        element canvasContainer,

                        UI.div #. "cellSelectConsole" #+
                            [element cellSel,
                            element console]
                    ]
 
                ]

    getBody window #+ [ pure body ]

    let
        clickReset :: Event Comm 
        clickReset = const (Restart fileEnv) <$> UI.click reset

        clickCanvas :: Event Comm
        clickCanvas =   (\pos -> UpdatePos pos) <$>
                        (\pos -> getIndex canvas (fst pos) (snd pos) cellSize) <$>
                        UI.mousedown canvas

        -- Generamos una lista de pares con los divs de los botones de celulas y los comandos (Select cId) asociados
        cellComm ::  [(Element, Comm)]
        cellComm = zip (map fst cellButtPairL) (map (\cell -> Select (cId cell)) (drop 1 (fst $ snd fileEnv)))

        clickCell :: Event Comm
        clickCell = (foldr1 (UI.unionWith const) . map makeClick) cellComm
                    where
                        makeClick (elmnt, cmd) = UI.pure cmd <@ UI.click elmnt

        timerTick :: Event Comm
        timerTick = const Step <$> UI.tick timer -- cambiar update por step

        interactions :: Event Comm
        interactions = foldr1 (UI.unionWith const) [clickReset, clickCanvas, clickCell, timerTick]

        commands :: Event (Either Error Env -> Either Error Env)
        commands = fmap evalUp interactions

        commandsArray :: Event ([UI Element] -> [UI Element])
        commandsArray = fmap commToDiv interactions

    calcBehaviour <- accumB (Right fileEnv) commands -- aca hay que checkear si el env de entrada es correcto

    -- comHist <- accumB empty commandsArray 



    element body # sink detectError calcBehaviour

    element canvas # sink updateCanvas calcBehaviour

    -- element console # sink updateConsole comHist

commToDiv :: Comm -> [UI Element] -> [UI Element]
commToDiv (Restart _) xs = empty
commToDiv x xs = (UI.div # set text (show x)):xs

updateConsole :: WriteAttr Element [UI Element]
updateConsole = mkWriteAttr $ \comHist console -> do
    element console # set children []
    element console #+ comHist
    return ()

drawCellList :: [CellData] -> CellData -> UI [(Element, Element)]
drawCellList (dCell:cL) selected =  do  cellButtPairL <- drawCellList' cL selected
                                        let
                                            cellButtL = map fst cellButtPairL
                                            cellButLabelL = map snd cellButtPairL
                                        -- Select behaviour
                                        forM_ cellButtPairL $ \(cellDiv, cellLab) ->
                                            on UI.click cellDiv $ const $ do
                                                forM_ cellButtPairL $ \(cellD, cellL) -> do
                                                    element cellD # set (attr "class") "item"
                                                    element cellL # set (attr "class") "ui label"
                                                element cellDiv # set (attr "class") "item active"
                                                element cellLab # set (attr "class") "ui left pointing label"
                                        return cellButtPairL

drawCellList' :: [CellData] -> CellData -> UI [(Element, Element)]
drawCellList' [] selected = return empty
drawCellList' (c:cs) selected = do  let nombre = name c
                                        id = cId c
                                        itemSel = if c == selected then "item active" else "item"
                                        labelSel = if c == selected then "ui left pointing label" else "ui label"
                                        color = colour c

                                    cellDiv <- UI.a #. itemSel
                                        # set UI.text (map toUpper nombre)
                                        # set style [("font-size", "20px")]
                                    cellLabel <- UI.div #. labelSel
                                                        # set style [("background-color", color), ("min-height", "20px")]
                                    element cellDiv #+ [element cellLabel]

                                    cellDivs <- drawCellList' cs selected
                                    return $ (cellDiv, cellLabel) : cellDivs

-- Actualiza el canvas con el estado actual del enviroment
updateCanvas :: WriteAttr Element (Either Error Env)
updateCanvas = mkWriteAttr $ \either canvas ->
    case either of
        Left err -> return ()
        Right env -> do
            let gridD = fst env
                cells = snd env
                autGrid = grid gridD
                changedCells = changes gridD
            -- canvas # UI.clearCanvas

            forM_ changedCells $ \(x, y)  -> do
            -- forM_ [0.. height gridD-1] $ \y -> do
            --     forM_ [0.. width gridD-1] $ \x -> do
                let cellId = (autGrid V.! y) V.! x
                    cell = searchCellId env cellId
                case cell of
                    Nothing -> return ()
                    Just c -> do
                        let color = colour c
                        drawSquare canvas (fromIntegral x * cellSize) (fromIntegral y * cellSize) cellSize color
                
-- Detecta el error en el step e inhabilita la UI
detectError :: WriteAttr Element (Either Error Env)
detectError = mkWriteAttr $ \either body -> do
    case either of
        Left err -> do
            element body # set style [("pointer-events","none")]
            element body # set style [("cursor", "not-allowed"),
                                    ("border", "5px solid red")]
            return ()
        Right _ -> return () 

drawSquare :: Canvas -> Double -> Double -> Double -> String -> UI()
drawSquare canvas x y size colour =
    let newX =  fromIntegral (floor (x/cellSize)) * cellSize + 1
        newY =  fromIntegral (floor (y/cellSize)) * cellSize + 1
    in  do  canvas # set' UI.fillStyle (UI.htmlColor colour)
            canvas # UI.fillRect (newX,newY) (size-2) (size-2)

getIndex :: Canvas -> Double -> Double -> Double -> Pos
getIndex canvas x y size =
    let newX =  floor (abs (x)/cellSize)
        newY =  floor (abs (y)/cellSize)
    in (newX, newY)

fromIntegralPoint :: (Int, Int) -> (Double, Double)
fromIntegralPoint (x, y) =  (fromIntegral x, fromIntegral y)


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
    canvas <- UI.canvas #. "canvas shadow"
        # set UI.height canvasSize
        # set UI.width  canvasSize

    -- Canvas container
    canvasContainer <- UI.div #. "canvas-container main"
        # set style [("height", show (canvasSize+1) ++ "px"), ("width", show (canvasSize+1) ++ "px")]
        # set UI.draggable False
        #+ [element canvas, element canvasBase]
    return (canvasContainer, canvas)

timeController :: UI.Timer -> Element -> [(Element, Element)] -> UI (Element, Element)
timeController timer console bs = do
    playContainer <- UI.div #. "ui vertical menu"

    play <- UI.a #. "icon-container item" #+ [
        UI.img # set UI.src "static/play.svg" #. "icon",
        string "Play"]

    pause <- UI.a #. "icon-container item" #+ [
        UI.img # set UI.src "static/pause.svg" #. "icon",
        string "Pause"
        ] # set style [("display", "none")]

    reset <- UI.button  #. "ui red button"
                        #+ [string "Reset"]

    on UI.click play $ const $ do 
        element play # set style [("display", "none")]
        element pause # set style [("display", "block")]
        UI.start timer
    
    on UI.click pause $ const $ do 
        element play # set style [("display", "block")]
        element pause # set style [("display", "none")]
        UI.stop timer        

    on UI.click reset $ const $ do
        UI.stop timer
        element play # set style [("display", "block")]
        element pause # set style [("display", "none")]
        -- seteamos la lista de botones a inactivos y activamos el primero
        forM_ bs $ \(cellDiv, cellLab) -> do
            element cellDiv # set (attr "class") "item"
            element cellLab # set (attr "class") "ui label"
        let (cDiv, cLab) = head bs
        element cDiv # set (attr "class") "item active"
        element cLab # set (attr "class") "ui left pointing label"
        element console # set children []

    element playContainer #+ [element play, element pause]
    return (playContainer, reset)