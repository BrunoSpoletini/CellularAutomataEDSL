module BehaviourManager where

import qualified Graphics.UI.Threepenny      as UI
import           Graphics.UI.Threepenny.Core hiding (grid)
import           Graphics.UI.Threepenny.Canvas as Canvas

import Data.Char (toUpper, toLower)

import Common
import Automata
import Monads
import Config

clickReset :: Env -> Element -> Event Comm 
clickReset env reset = const (Restart env) <$> UI.click reset

clickCanvas :: Canvas -> Event Comm
clickCanvas canvas =    (\pos -> UpdatePos pos) <$>
                        (\pos -> getIndex canvas (fst pos) (snd pos) cellSize) <$>
                        UI.mousedown canvas

cells :: [(String, Env)] -> [CellData]
cells envs =  concat (map (\(fileName, (gridData, (cellsD, sel))) -> drop 1 cellsD) envs)

-- Generamos una lista de pares con los divs de los botones de celulas y los comandos (Select cId) asociados
cellComm ::  [Element] -> [(String, Env)] -> [(Element, Comm)]
cellComm cellButtons envs = zip cellButtons (map (\cell -> Select (Var $ name cell)) (cells envs))

envComm :: [Element] -> [(String, Env)] -> [(Element, Comm)]
envComm envSelList envs = zip envSelList (map (\(_, e) -> Restart e) envs)

makeClick :: (Element, Comm) -> Event Comm
makeClick (elmnt, cmd) = UI.pure cmd <@ UI.click elmnt

clickEnv :: [Element] -> [(String, Env)] -> Event Comm
clickEnv envSelList envs = (foldr1 (UI.unionWith const) . map makeClick) (envComm envSelList envs)

clickCell :: [Element] -> [(String, Env)] -> Event Comm
clickCell cellButtons envs = (foldr1 (UI.unionWith const) . map makeClick) (cellComm cellButtons envs)

timerTick :: UI.Timer -> Event Comm
timerTick timer = const Step <$> UI.tick timer

getInteractions :: (Env, [(String, Env)], [Element], [Element], Element, Canvas, UI.Timer) -> Event Comm
getInteractions (env, envs, cellButtons, envSelList, reset, canvas, timer ) = 
    foldr1 (UI.unionWith const) [
        clickReset env reset, 
        clickCanvas canvas, 
        clickCell cellButtons envs, 
        timerTick timer, 
        clickEnv envSelList envs ]

getTransitionEvents :: Event Comm -> Event (Either Error Env -> Either Error Env)
getTransitionEvents interactions = fmap evalUp interactions

getCommandsEvents :: Event Comm -> Event ([Comm] -> [Comm])
getCommandsEvents interactions = fmap (:) interactions

-- // Funciones auxiliares

getIndex :: Canvas -> Double -> Double -> Double -> Pos
getIndex canvas x y size =
    let newX =  floor (abs (x)/cellSize)
        newY =  floor (abs (y)/cellSize)
    in (newX, newY)

-- -- Obtiene la celula seleccionada
-- getSelectedCell :: Behavior (Either Error Env) -> UI String
-- getSelectedCell calcBehaviour = do
--     currentState <- currentValue calcBehaviour
--     case currentState of
--         Left err -> return "Error"
--         Right env -> return $ name $ snd $ snd env

-- getCurrentComms :: Behavior (Either Error Env) -> UI [Comm]
-- getCurrentComms calcBehaviour = do
--     currentState <- currentValue calcBehaviour
--     case currentState of
--         Left err -> return []
--         Right env -> return $ snd $ snd env

