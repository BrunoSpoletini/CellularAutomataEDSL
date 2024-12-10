module Components
where

import Control.Monad -- (forM_, mapM_)

import qualified Graphics.UI.Threepenny      as UI
import           Graphics.UI.Threepenny.Core hiding (grid)
import           Graphics.UI.Threepenny.Canvas as Canvas
import qualified Data.Vector as V
import Data.Char

import Common
import Automata
import Config

-- // Selectores de celulas //

drawManyCellList :: [[CellData]] -> Bool -> UI ([Element],[Element])
drawManyCellList [] disp = return ([],[])
drawManyCellList (l:ls) disp = do
    let display = if disp then "block" else "none"
    cellButtons <- drawCellList l
    cellSelection <- UI.div #. "ui vertical menu"
                            # set style [("display", display)]
                            #+ (  map element $ cellButtons )
    (cellButtonsRest, cellSelections) <- drawManyCellList ls False
    return $ (concat [cellButtons, cellButtonsRest], cellSelection : cellSelections)

drawCellList :: [CellData] -> UI [Element]
drawCellList [] =  return []
drawCellList (dCell:cL) =  do  
    cellButtons <- drawCellButtons cL 
    element (head cellButtons) # set (attr "class") "item active"
    forM_ cellButtons $ \cellDiv ->
        on UI.click cellDiv $ const $ do
            forM_ cellButtons $ \cellD -> do
                element cellD # set (attr "class") "item"
            element cellDiv # set (attr "class") "item active"
    return cellButtons

drawCellButtons :: [CellData] -> UI [Element]
drawCellButtons [] = return []
drawCellButtons (c:cs) = do  
    let nombre = name c
        color = colour c
    cellDiv <- UI.a #. "item"
                    # set UI.text (map toUpper nombre)
                    # set style [("font-size", "20px")]
    cellLabelActive <- UI.div   #. "ui left pointing label"
                                # set style [("background-color", show color)]
    cellLabelInactive <- UI.div #. "ui label"
                                # set style [("background-color", show color)]
    element cellDiv #+ [element cellLabelActive, element cellLabelInactive]
    cellDivs <- drawCellButtons cs
    return $ cellDiv : cellDivs

-- // Dibujo de la grilla //

-- Dibuja todo el canvas
drawCanvas :: Int -> Int -> Env -> UI (Element, Element)
drawCanvas cellSize canvasSize env = do
     -- Static Canvas --
    canvasBase <- UI.canvas #. "static-canvas"
        # set UI.height canvasSize
        # set UI.width  canvasSize
        # set UI.strokeStyle "black"
        # set UI.lineWidth 1

    -- Draw grid on Static Canvas
    forM_ [0,cellSize..canvasSize] $ \x -> do
        UI.moveTo (fromIntegralPoint (x, 0)) canvasBase
        UI.lineTo (fromIntegralPoint (x, canvasSize)) canvasBase
        UI.stroke canvasBase
        UI.moveTo (fromIntegralPoint (0, x)) canvasBase
        UI.lineTo (fromIntegralPoint (canvasSize, x)) canvasBase
        UI.stroke canvasBase

    -- Volatile Canvas
    canvas <- UI.canvas #. "canvas shadow"
        # set UI.height canvasSize
        # set UI.width  canvasSize

    -- Draw the actual state of the grid
    drawSquares canvas env

    -- Canvas container
    canvasContainer <- UI.div #. "canvas-container main"
        # set style [("height", show (canvasSize+1) ++ "px"), ("width", show (canvasSize+1) ++ "px")]
        # set UI.draggable False
        #+ [element canvas, element canvasBase]
    return (canvasContainer, canvas)

-- Dibuja las celulas en el canvas segun los ultimos cambios realizados en la grid
drawSquaresOpt :: Canvas -> Env -> UI ()
drawSquaresOpt canvas env = do
    let gridD = fst env
        autGrid = grid gridD
        changedCells = changes gridD
    forM_ changedCells $ \(x, y)  -> do
        let cellId = (autGrid V.! y) V.! x
            cell = searchCellId env cellId
        case cell of
            Nothing -> return ()
            Just c -> do
                let color = colour c
                drawSquare canvas (fromIntegral x * cellSize) (fromIntegral y * cellSize) cellSize color

-- Dibuja las celulas en el canvas segun cada elemento de la grid
drawSquares :: Canvas -> Env -> UI ()
drawSquares canvas env = do
    let gridD = fst env
        autGrid = grid gridD
    forM_ [0..(V.length autGrid - 1)] $ \y -> do
        forM_ [0..(V.length (autGrid V.! y) - 1)] $ \x -> do
            let cellId = (autGrid V.! y) V.! x
                cell = searchCellId env cellId
            case cell of
                Nothing -> return ()
                Just c -> do
                    let color = colour c
                    drawSquare canvas (fromIntegral x * cellSize) (fromIntegral y * cellSize) cellSize color

-- Dibuja un cuadrado en la grid
drawSquare :: Canvas -> Double -> Double -> Double -> Common.Color -> UI()
drawSquare canvas x y size colour =
    let newX =  fromIntegral (floor (x/cellSize)) * cellSize + 1
        newY =  fromIntegral (floor (y/cellSize)) * cellSize + 1
    in  do  canvas # set' UI.fillStyle (UI.htmlColor $ show colour)
            canvas # UI.fillRect (newX,newY) (size-2) (size-2)


-- // Controlador de los botones //
-- Dibuja la botonera de play stop y restart
timeController :: UI.Timer -> Element -> [Element] -> [Element] -> [(String, Env)] -> UI (Element, Element)
timeController timer console cellButtons cellSelectors envs = do
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
        resetCellSelector cellSelectors (head cellSelectors) cellButtons envs

    element playContainer #+ [element play, element pause]
    return (playContainer, reset)

-- // Selector de entornos //

-- Dibuja el selector de entornos
envSelector ::  [(String, Env)] -> [Element] -> [Element] -> Env -> UI (Element, [Element])
envSelector envs cellSelectors cellButtons fileEnv = do
    envSel <- UI.div #. "enviromentSelector"
    envOpts <- envSelectorOptions envs
    
    -- Ocultamos todos los selectores de celulas excepto el que usa nuestro enviroment
    forM_ (zip envOpts cellSelectors) $ \(opt, cellSelector) -> do
        on UI.click opt $ const $ do
            resetCellSelector cellSelectors cellSelector cellButtons envs

    element envSel #+ (map element envOpts)
    return (envSel, envOpts)

-- Dibuja las opciones del selector de entornos
envSelectorOptions :: [(String, Env)] -> UI [Element]
envSelectorOptions [] = return empty
envSelectorOptions envsP@((name, env):es) = do
    (canvasContainer, canvas) <- drawCanvas (floor cellSize) (floor canvasSize) env
    let nameUpper = map toUpper $ take ((length name) - 4) name
        isDefault = if nameUpper == "DEFAULT" then "none" else "flex"
    option <- UI.div #. "enviroment"
                     #+ [   UI.div #. "enviromentName" # set text nameUpper,
                            element canvasContainer
                        ]
                     # set style [("display", isDefault)]
    envOpts <- envSelectorOptions es
    return $ option:envOpts

-- Oculta todos los selectores de celulas, muestra el seleccionado, y setea el primer
-- boton de cada selector de celula como activo
resetCellSelector  :: [Element] -> Element -> [Element] -> [(String, Env)] -> UI ()
resetCellSelector cellSelectors selectedSelector cellButtons envs = do
    forM_ cellSelectors $ \cellSel -> do
        element cellSel # set style [("display", "none")]
    element selectedSelector # set style [("display", "block")]

    let cellOptionsLengths = map (\(name, env) -> length (fst (snd env)) - 1) envs

    forM_ cellButtons $ \cellDiv -> do
        element cellDiv # set (attr "class") "item"

    forM_ [0 .. ((length cellSelectors)-1)] $ \i -> do
         element (cellButtons!!((foldr (+) 0 (take i cellOptionsLengths)))) # set (attr "class") "item active"
    return ()


getConsoleDisplay :: UI Element
getConsoleDisplay = do
    consoleDisp <- UI.div   #. "ui segment console"
                            # set (attr "contenteditable") "false"
    return consoleDisp

-- // Dibujo del boton de exportar
getExportButton :: String -> UI Element
getExportButton hist = do
    button <- UI.div    #. "ui button export"
                        # set UI.text "Export to out.txt"
    on UI.click button $ const $ do
        liftIO $ writeFile outFile hist
        element button # set UI.text "Exported!"
                       # set style [("background-color", "#5cb85c")]
    return button

-- // Funciones auxiliares

-- Inhabilita la GUI
handleError :: Element -> UI ()
handleError body = do
    element body # set style [("pointer-events","none")]
    element body # set style [("cursor", "not-allowed"),
                            ("border", "5px solid red")]
    return ()

-- Auxiliar puntos del canvas
fromIntegralPoint :: (Int, Int) -> (Double, Double)
fromIntegralPoint (x, y) =  (fromIntegral x, fromIntegral y)
