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

import Config


setupFront :: Window -> Env -> [(String, Env)] -> UI ()
setupFront window fileEnv envs = void $ do
    UI.addStyleSheet window "grid.css"
    UI.addStyleSheet window "semantic.min.css"
    pure window # set UI.title "Cellular Automata"
    url <- UI.loadFile "image/png" "static/triangulos.png"

    --let fileEnv = snd (envs!!0)--snd $ filter (\(n, _) -> n == "default.txt") envs !! 0

    -- Header
    header <- UI.div #. "header"
                     # set text "Cellular Automata"

    -- Canvas
    (canvasContainer, canvas) <- drawCanvas (floor cellSize) (floor canvasSize) fileEnv
    
    -- Selector de celulas
    cellButtons <- drawCellList (fst $ snd fileEnv) (snd $ snd fileEnv) maxCells
    cellSel <- UI.div #. "ui vertical menu"
                      #+ (  map element $ cellButtons )

    -- Console
    console <- UI.div #. "ui segment console"
        # set (attr "contenteditable") "false"
        # set text "Historial de comandos"

    -- Timer
    timer <- UI.timer
            # set UI.interval 1000
            # set UI.running False

    -- Botones de control
    (playContainer, reset) <- timeController timer console cellButtons

    -- Selector de entorno
    (envSel, envSelList) <- envSelector envs fileEnv

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



    --actualEnv :: Env
    --actualEitherEnv <- currentValue calcBehaviour


    let

        -- actualEnv = case actualEitherEnv of
        --     Left _ -> fileEnv
        --     Right e -> e

        clickReset :: Event Comm 
        clickReset = const (Restart fileEnv) <$> UI.click reset

        clickCanvas :: Event Comm
        clickCanvas =   (\pos -> UpdatePos pos) <$>
                        (\pos -> getIndex canvas (fst pos) (snd pos) cellSize) <$>
                        UI.mousedown canvas

        -- Generamos una lista de pares con los divs de los botones de celulas y los comandos (Select cId) asociados


        cellComm ::  [(Element, Comm)]
        cellComm = zip cellButtons (map (\cell -> Select (name cell)) (drop 1 (fst $ snd fileEnv)))

        envComm :: [(Element, Comm)]
        envComm = zip envSelList (map (\(_, e) -> Restart e) envs)

        clickEnv :: Event Comm
        clickEnv = (foldr1 (UI.unionWith const) . map makeClick) envComm
                    where
                        makeClick (elmnt, cmd) = UI.pure cmd <@ UI.click elmnt

        clickCell :: Event Comm
        clickCell = (foldr1 (UI.unionWith const) . map makeClick) cellComm -- TO DO fix duplicated code
                    where
                        makeClick (elmnt, cmd) = UI.pure cmd <@ UI.click elmnt

        timerTick :: Event Comm
        timerTick = const Step <$> UI.tick timer

        interactions :: Event Comm
        interactions = foldr1 (UI.unionWith const) [clickReset, clickCanvas, clickCell, timerTick, clickEnv]

        commands :: Event (Either Error Env -> Either Error Env)
        commands = fmap evalUp interactions

        commandsArray :: Event ([Comm] -> [Comm])
        commandsArray = fmap (:) interactions

        -- commands :: (MonadState m, MonadError m) => Event (m () -> m ())
        -- commands = fmap (>>) interactions --TO DO considerar este cambio
        
    -- calcBehaviour :: Behavior (Either Error Env)
    calcBehaviour <- accumB (Right fileEnv) commands -- aca hay que checkear si el env de entrada es correcto

    comHist <- accumB [] commandsArray 

    element body # sink detectError calcBehaviour

    element canvas # sink updateCanvas calcBehaviour





    -- actualizar la lista de celulas
    --element cellSel # sink updateCellList calcBehaviour
    sink updateCellList calcBehaviour $ pure cellButtons

    -- cellButtons

    element console # sink updateConsole comHist




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

drawCellList :: [CellData] -> CellData -> Int -> UI [Element]
drawCellList (dCell:cL) selected maxCells =  do  
    cellButtons <- drawCellList' cL selected maxCells
    forM_ cellButtons $ \cellDiv ->
        on UI.click cellDiv $ const $ do
            forM_ cellButtons $ \cellD -> do
                element cellD # set (attr "class") "item"
            element cellDiv # set (attr "class") "item active"
    return cellButtons

drawCellList' :: [CellData] -> CellData -> Int -> UI [Element]
drawCellList' _ selected 0 = return []
drawCellList' [] selected maxCells = do
    cellDiv <- UI.a #. "item"
        # set style [("font-size", "20px"), ("display", "none")]
    cellDivs <- drawCellList' [] selected (maxCells - 1)
    return $ cellDiv : cellDivs
drawCellList' (c:cs) selected maxCells = do  
    let nombre = name c
        itemSel = if c == selected then "item active" else "item"
        color = colour c
    cellDiv <- UI.a #. itemSel
        # set UI.text (map toUpper nombre)
        # set style [("font-size", "20px")]
    cellLabelActive <- UI.div   #. "ui left pointing label"
                                # set style [("background-color", color)]
    cellLabelInactive <- UI.div #. "ui label"
                                # set style [("background-color", color)]
    element cellDiv #+ [element cellLabelActive, element cellLabelInactive]
    cellDivs <- drawCellList' cs selected (maxCells - 1)
    return $ cellDiv : cellDivs

updateCellList :: WriteAttr [Element] (Either Error Env)
updateCellList = mkWriteAttr $ \either cellButtons -> do
    case either of
        Left err -> return ()
        Right env -> do
            let cellData = drop 1 $ fst $ snd env
                selectedCell = snd $ snd env
                cellPair = zip cellData cellButtons
            forM_ cellButtons $ \but -> do
                element but # set style [("display", "none")]
            forM_ cellPair $ \(cell, but) -> do
                element but # set children []
                let color = colour cell
                cellLabelActive <- UI.div   #. "ui left pointing label"
                                            # set style [("background-color", color)]
                cellLabelInactive <- UI.div #. "ui label"
                                            # set style [("background-color", color)]  
                element but # set UI.text (map toUpper $ name cell)
                element but # set style [("display", "block")]
                element but #+ [element cellLabelActive, element cellLabelInactive]
            
            return ()


-- Actualiza el canvas con el estado actual del enviroment
updateCanvas :: WriteAttr Element (Either Error Env)
updateCanvas = mkWriteAttr $ \either canvas ->
    case either of
        Left err -> return ()
        Right env -> drawSquaresOpt canvas env

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

timeController :: UI.Timer -> Element -> [Element] -> UI (Element, Element)
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
        forM_ bs $ \cellDiv -> do
            element cellDiv # set (attr "class") "item"
        let cDiv = head bs
        element cDiv # set (attr "class") "item active"
        element console # set children []


    element playContainer #+ [element play, element pause]
    return (playContainer, reset)


envSelector ::  [(String, Env)] -> Env -> UI (Element, [Element])
envSelector envs fileEnv = do
    envSel <- UI.div #. "enviromentSelector"
    envOpts <- envSelector' envs
    
    -- forM_ (zip envOpts envs) $ \(opt, env) -> do
    --     on UI.click opt $ const $ do
    --         window <- askWindow
    --         getBody window # set children []
    --         setupFront window (snd env) envs

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


    