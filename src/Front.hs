{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# LANGUAGE RecursiveDo #-}

module Front
    where

-- import Data.Char (toUpper, toLower)
import Data.IORef
import Control.Monad -- (forM_, mapM_)
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

import Components
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
        Right env -> drawSquaresOpt canvas env

updateConsole :: WriteAttr Element [Comm]
updateConsole = mkWriteAttr $ \comHist console -> do
    element console # set children []
    element console # set text (commToString comHist)
    return ()