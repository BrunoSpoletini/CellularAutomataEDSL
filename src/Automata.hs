module Automata where

import Common
import MonadAut
import Prelude
import Control.Monad
-- import qualified Data.Map.Strict as M
import Data.Strict.Tuple hiding (fst, snd)
import qualified Data.Vector as V


-- Va a tener una version de lo siguiente: EN DESARROLLO

-- Enviroments
-- type Env = (GridData, [CellData]) -- declared in common

-- -- State Monad with Error Handler
-- newtype StateError a =
--   StateError { runStateError :: Env -> Either Error (Pair a Env) }

-- -- Para calmar al GHC
-- instance Functor StateError where
--   fmap = liftM

-- instance Applicative StateError where
--   pure  = return
--   (<*>) = ap

-- instance Monad StateError where
--   return x = StateError (\s -> Right (x :!: s))
--   m >>= f = StateError (\s ->   let e = runStateError m s 
--                                 in case e of
--                                     (Left err) -> Left err
--                                     (Right (v :!: s')) -> runStateError (f v) s')

-- instance MonadError StateError where
--   throw e = StateError(\s -> Left e)

{-
checkCell :: Pos -> Env -> String
checkCell pos env = case runAut (checkGrid pos) env of
                      Left err -> "Error: " ++ show err
                      Right (cellId :!: env) -> show cellId


eval :: Comm -> Env -> Either Error Env
eval c env =  case runStateError (processComm c) env of
                  (Left err) -> Left err
                  (Right (v :!: s)) -> Right s

processComm :: MonadAut m => Comm -> m ()
processComm (UpdateCell pos name) = do  cellData <- lookforCell (Var name)
                                        updateGrid pos (cId cellData)                           
processComm (DefCell name col xs ys) = addCell name col xs ys
-- processComm Step = 

searchCellId :: Env -> CellId -> Maybe CellData
searchCellId (gData, []) idCell = Nothing
searchCellId (gData, c:cl) idCell =   if cId c == idCell then Just c 
                                            else searchCellId (gData, cl) idCell

searchCellName :: Env -> Variable -> Maybe CellData
searchCellName (gData, []) var = Nothing
searchCellName (gData, c:cl) var =   if name c == var then Just c 
                                            else searchCellName (gData, cl) var

changeCell :: CellId -> Pos -> GridData -> Maybe GridData
changeCell id (x, y) g =    if x > width g || x < 0 || y > height g || y < 0 then
                                Nothing
                            else
                                let gr = grid g
                                    upper = V.take (y-1) gr
                                    lower = V.drop x gr
                                    middleRow = gr V.! y
                                    left = V.take (x-1) middleRow
                                    right = V.drop x middleRow
                                    m = V.singleton id
                                    newMiddleRow = V.singleton (left V.++ m V.++ right)
                                in Just g {grid = upper V.++ newMiddleRow V.++ lower}

createCell :: [CellData] -> Variable -> Variable -> [Int] -> [Int] -> CellData
createCell (c:cs) n col xs ys = CellData {  cId = cId c + 1, 
                                            name = n, 
                                            colour = col, 
                                            bornL = xs, 
                                            surviveL  = ys }



-}                                            