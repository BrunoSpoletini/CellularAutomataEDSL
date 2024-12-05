{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# LANGUAGE RecursiveDo #-}

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

import System.Directory 

import Common
import Automata
import Monads

import BehaviourManager
import Config


setupFront :: Window -> [(String, Env)] -> UI ()
setupFront window envs = void $ do
    UI.addStyleSheet window "grid.css"
    UI.addStyleSheet window "semantic.min.css"
    pure window # set UI.title "Cellular Automata"
    url <- UI.loadFile "image/png" "static/triangulos.png"

    let fileEnv = snd $ filter (\(n, _) -> n == "default.txt") envs !! 0

    -- Header
    header <- UI.div #. "header" # set text "Cellular Automata"

    -- Canvas
    (canvasContainer, canvas) <- drawCanvas (floor cellSize) (floor canvasSize) fileEnv
    
    -- Selector de celulas
    (cellButtons, cellSelectors) <- drawManyCellList ((map (fst.snd.snd) envs)) True
    cellSel <- UI.div #+ (  map element $ cellSelectors )

    -- Console
    console <- UI.div #. "ui segment console"
        # set (attr "contenteditable") "false"
        # set text "Historial de comandos"

    -- Timer
    timer <- UI.timer
            # set UI.interval 1000
            # set UI.running False

    -- Botones de control
    (playContainer, reset) <- timeController timer console cellButtons cellSelectors envs

    -- Selector de entorno
    (envSel, envSelList) <- envSelector envs cellSelectors cellButtons fileEnv

    body <- UI.div #. "page-container" 
                   # set UI.style [("background-image", "url(" ++ url ++ ")"), ("background-size", "cover")]
                #+ [
                    element header,
                    UI.div #. "displayRow" #+ 
                    [
                        UI.div #. "leftContainer" #+
                            [ UI.div #. "menu"
                                #+ [element playContainer, element reset],
                                element envSel
                            ],
                        element canvasContainer,

                        UI.div #. "cellSelectConsole" #+
                            [element cellSel,
                            element console]
                    ]
                ]
    getBody window #+ [ pure body ]

    -- Manejador de Eventos

    let datos = (fileEnv, envs, cellButtons, envSelList, reset, canvas, timer)
        interactions = getInteractions datos
        transitionEvents = getTransitionEvents interactions
        commandsEvents = getCommandsEvents interactions
        
    calcBehaviour <- accumB (Right fileEnv) transitionEvents
    comHistBehaviour <- accumB [] commandsEvents 

    -- Actualizamos la GUI
    sink updateConsole comHistBehaviour $ pure console
    sink updateGUI calcBehaviour $ pure (canvas, cellButtons, envs, body)

updateGUI :: WriteAttr (Canvas, [Element], [(String, Env)], Element) (Either Error Env)
updateGUI = mkWriteAttr $ \either (canvas, cellButtons, envs, body) -> do
    case either of
        Left err -> handleError body
        Right env -> do
            drawSquaresOpt canvas env
            --updateSelectedCell cellButtons env envs
            return ()

updateSelectedCell :: [Element] -> Env -> [(String, Env)] -> UI ()
updateSelectedCell cellButtons env envsP = do
    let --cellData = drop 1 $ fst $ snd env
        selectedCell = snd $ snd env
        totalCellList = concat (map (fst.snd.snd) envsP)
    forM_ (zip cellButtons totalCellList) $ \(cellDiv, cellData) -> do
        if (name cellData) == (name selectedCell) then
            element cellDiv # set (attr "class") "item active test"
        else
            element cellDiv # set (attr "class") "item"
    return ()


commToString :: [Comm] -> String
commToString [] = ""
commToString ((Restart e):cs) = ""
commToString (Step:cs) = resumeSteps cs 1
commToString (Select n:cs) = "Select " ++ (map toUpper n) ++ "\n" ++ (commToString cs)
commToString (c:cs) = (show c) ++ "\n" ++ (commToString cs)

resumeSteps :: [Comm] -> Int -> String
resumeSteps (Step:cs) n = resumeSteps cs (n+1)
resumeSteps (cs) 1 = "Step\n" ++ commToString cs
resumeSteps (cs) n = "Step (x" ++ (show n) ++ ")\n" ++ (commToString cs)

updateConsole :: WriteAttr Element [Comm]
updateConsole = mkWriteAttr $ \comHist console -> do
    element console # set children []
    element console # set text (commToString comHist)
    return ()

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
                                # set style [("background-color", color)]
    cellLabelInactive <- UI.div #. "ui label"
                                # set style [("background-color", color)]
    element cellDiv #+ [element cellLabelActive, element cellLabelInactive]
    cellDivs <- drawCellButtons cs
    return $ cellDiv : cellDivs


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

-- Inhabilita la GUI
handleError :: Element -> UI ()
handleError body = do
    element body # set style [("pointer-events","none")]
    element body # set style [("cursor", "not-allowed"),
                            ("border", "5px solid red")]
    return ()


drawSquare :: Canvas -> Double -> Double -> Double -> String -> UI()
drawSquare canvas x y size colour =
    let newX =  fromIntegral (floor (x/cellSize)) * cellSize + 1
        newY =  fromIntegral (floor (y/cellSize)) * cellSize + 1
    in  do  canvas # set' UI.fillStyle (UI.htmlColor colour)
            canvas # UI.fillRect (newX,newY) (size-2) (size-2)

fromIntegralPoint :: (Int, Int) -> (Double, Double)
fromIntegralPoint (x, y) =  (fromIntegral x, fromIntegral y)

-- Draw the whole canvas
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
        element console # set children []

    element playContainer #+ [element play, element pause]
    return (playContainer, reset)


envSelector ::  [(String, Env)] -> [Element] -> [Element] -> Env -> UI (Element, [Element])
envSelector envs cellSelectors cellButtons fileEnv = do
    envSel <- UI.div #. "enviromentSelector"
    envOpts <- envSelector' envs
    
    -- Ocultamos todos los selectores de celulas excepto el que usa nuestro enviroment
    forM_ (zip envOpts cellSelectors) $ \(opt, cellSelector) -> do
        on UI.click opt $ const $ do
            resetCellSelector cellSelectors cellSelector cellButtons envs

    element envSel #+ (map element envOpts)
    return (envSel, envOpts)

envSelector' :: [(String, Env)] -> UI [Element]
envSelector' [] = return empty
envSelector' envsP@((name, env):es) = do
    (canvasContainer, canvas) <- drawCanvas (floor cellSize) (floor canvasSize) env

    option <- UI.div #. "enviroment"
                     #+ [   UI.div #. "enviromentName" # set text (map toUpper $ take ((length name) - 4) name),
                            element canvasContainer
                        ]

    envOpts <- envSelector' es
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

