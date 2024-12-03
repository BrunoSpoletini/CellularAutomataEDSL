module Automata where
import Debug.Trace -- TO DO remove
import Common
import Monads
import Prelude
import Control.Monad
import Data.Char
import Parse
-- import qualified Data.Map.Strict as M
import Data.Strict.Tuple hiding (fst, snd)
import qualified Data.Vector as V

-- Enviroments
-- type Env = (GridData, ([CellData], CellData))

cellSize = 20 :: Double --20
canvasSize = 500 :: Double --Default: 500

-- Init enviroment
initEnv :: Env
initEnv   = let size =  floor(canvasSize/cellSize)
                deadCell = CellData { cId = 0, 
                                    name = "dead", 
                                    colour = "rgb(240 241 236)", 
                                    bornL = [], 
                                    surviveL  = [1,2,3,4,5,6,7,8] }
                conwayCell = CellData { cId = 1, 
                              name = "conway", 
                              colour = "black", 
                              bornL = [3], 
                              surviveL  = [2,3] } 
                -- highlifeCell = CellData { cId = 2, 
                --               name = "highlife", 
                --               colour = "blue",
                --               bornL = [3,6], 
                --               surviveL  = [2,3] }
                -- seedsCell = CellData { cId = 3, 
                --               name = "seeds", 
                --               colour = "red",
                --               bornL = [2], 
                --               surviveL  = [] }
                gridD = GridData { height = size, -- to be changed
                                width = size, -- to be changed
                                grid = V.fromList (replicate size (V.fromList (replicate size 0))),
                                limits = [0,0,0,0],
                                changes = []
                                }
            in (gridD, ([deadCell, conwayCell], conwayCell))--, highlifeCell, seedsCell

-- State Monad with Error Handler
newtype StateError a =
  StateError { runStateError :: Env -> Either Error (Pair a Env) }

instance Functor StateError where
  fmap = liftM

instance Applicative StateError where
  pure  = pure
  (<*>) = ap

instance Monad StateError where
  --return x = StateError (\s -> Right (x :!: s))
  m >>= f = StateError (\s ->   let e = runStateError m s 
                                in case e of
                                    (Left err) -> Left err
                                    (Right (v :!: s')) -> runStateError (f v) s')

instance MonadError StateError where
  throw e = StateError(\s -> Left e)

instance MonadState StateError where
    lookforCell ident = StateError (\s -> 
        case ident of
            Var name -> (case searchCellName s name of
                            Nothing -> Left UndefCell
                            Just cellData -> Right (cellData :!: s))
            Id id -> case searchCellId s id of
                        Nothing -> Left UndefCell
                        Just cellData -> Right (cellData :!: s) )

    checkGrid (x, y) = StateError(\s -> Right (((grid (fst s) V.! y) V.! x):!: s) )

    updateGrid (x, y) idCell = StateError(\s -> 
        case changeCell idCell (x, y) (fst s) of 
            Nothing -> Left OutOfBounds
            Just newGrid -> Right (() :!: (newGrid, snd s)))
                                         
    addCell var col xs ys = StateError(\s -> 
        case runStateError (lookforCell (Var var)) s of
            Left UndefCell -> Right (() :!: (fst s, (((fst (snd s) ++ [cell]), snd(snd s))))) 
                                where cell = createCell (fst (snd s)) var col xs ys
            Right x -> Left NameInUse
        )

    storeChanges cellL = StateError(\s -> Right (() :!: ((fst s) {changes = cellL}, snd s)))

    setEnv env = StateError(\s -> Right (() :!: env))

    getEnv = StateError(\s -> Right (s :!: s))

    getSel = StateError(\s -> Right (snd (snd s) :!: s))
    

checkCell :: Pos -> Env -> String
checkCell pos env = case runStateError (checkGrid pos) env of
                      Left err -> "Error: " ++ show err
                      Right (cellId :!: env) -> show cellId

-- Considera el estado inicial como un error
evalUp :: Comm -> Either Error Env -> Either Error Env
evalUp c (Left err) = Left err
evalUp c (Right env) = eval c env

eval :: Comm -> Env -> Either Error Env
eval c env =  case runStateError (processComm c) env of
                  (Left err) -> Left err
                  (Right (v :!: s)) -> Right s

processComm :: (MonadState m, MonadError m) => Comm -> m ()
processComm (UpdateCell pos name) = do  cellData <- lookforCell (Var name)
                                        updateGrid pos (cId cellData)                           
processComm (DefCell name col xs ys) = addCell name col xs ys
processComm (UpdatePos pos) = do    cellId <- checkGrid pos
                                    sel <- getSel
                                    if cId sel == cellId then -- Deseleccionar
                                        do  updateGrid pos 0
                                            storeChanges [pos]
                                    else 
                                        do  updateGrid pos (cId sel)
                                            storeChanges [pos]
processComm Step = resolveStep


processComm (Restart env) = do  let cuadr = grid (fst env)
                                setEnv env
                                storeChanges ([(i, j) | i <- [0..(V.length cuadr - 1)], j <- [0..(V.length (cuadr V.! 0) - 1)]])
processComm (Select name) = do  cellData <- lookforCell (Var name)
                                env <- getEnv
                                setEnv (fst env, (fst $ snd env, cellData))
processComm _ = return ()

loadMonad :: (MonadState m, MonadError m) => [Comm] -> m ()
loadMonad [] = return ()
loadMonad cs = foldr1 (>>) (map processComm cs)


searchCellId :: Env -> CellId -> Maybe CellData
searchCellId (gData, ([], _)) idCell = Nothing
searchCellId (gData, (c:cl, sel)) idCell =  if cId c == idCell then Just c 
                                            else searchCellId (gData, (cl, sel)) idCell

searchCellName :: Env -> Variable -> Maybe CellData
searchCellName (gData, ([], _)) var = Nothing
searchCellName (gData, (c:cl, sel)) var =   if name c == (map toLower var) then Just c 
                                            else searchCellName (gData, (cl, sel)) var

changeCell :: CellId -> Pos -> GridData -> Maybe GridData
changeCell id (x, y) g =    if x > width g || x < 0 || y > height g || y < 0 then
                                Nothing
                            else
                                let gr = grid g
                                    upper = V.take y gr
                                    lower = V.drop (y+1) gr
                                    middleRow = gr V.! y
                                    left = V.take x middleRow
                                    right = V.drop (x+1) middleRow
                                    m = V.singleton id
                                    newMiddleRow = V.singleton (left V.++ m V.++ right)
                                in Just g {grid = upper V.++ newMiddleRow V.++ lower}

createCell :: [CellData] -> Variable -> Variable -> [Int] -> [Int] -> CellData
createCell cs n col xs ys = CellData {  cId = cId (last cs) + 1, 
                                        name = (map toLower n), 
                                        colour = col, 
                                        bornL = xs, 
                                        surviveL  = ys }

printGrid :: Env -> String
printGrid (gData, cList) = let g = grid gData
                in V.foldl (\acc x -> acc ++ (V.foldl (\acc y -> acc ++ (show y) ++ " ") "" x) ++ "-\n") "" g

resolveStep :: (MonadState m, MonadError m) => m ()
resolveStep = do
    env <- getEnv
    let gData = fst env
        cuadr = grid gData
        newGrid = V.imap (\i row -> V.imap (\j cell -> resolveCell (i, j) cell env) row) cuadr
        changedCells = filter (\(x, y) -> ((cuadr V.! y) V.! x) /= (newGrid V.! y V.! x)) [(i, j) | i <- [0..(V.length cuadr - 1)], j <- [0..(V.length (cuadr V.! 0) - 1)] ]
    setEnv (gData {grid = newGrid, changes = changedCells}, snd env)

resolveCell :: Pos -> CellId -> Env -> CellId
resolveCell (x, y) currentCellId env = 
    let gData = fst env
        cellList = fst $ snd env
        maybeCellData = searchCellId env currentCellId
    in case maybeCellData of
        Nothing -> 0
        Just currentCellData ->   let neighbours = getNeighbours (x, y) gData
                    in if currentCellId == 0 then
                            cellBirth cellList currentCellData neighbours
                        else
                            if elem (length $ filter (==cId currentCellData) (getNeighbours (x, y) gData)) (surviveL currentCellData) then cId currentCellData else 0

cellBirth :: [CellData] -> CellData -> [CellId] -> CellId
cellBirth cells cellData neighbours = 
    let canBeBorn c = elem (length $ filter (== cId c) neighbours) (bornL c)
        bornable = filter canBeBorn cells
    in  if length bornable == 1 then -- si hay mas de una celula que pueda nacer, no nace ninguna
            cId $ head bornable 
        else 0

getNeighbours :: Pos -> GridData -> [CellId]
getNeighbours (x, y) gData = 
    let cuadr = grid gData
        height = V.length cuadr
        width = V.length (cuadr V.! 0)
        neighbours = [(x-1, y-1), (x, y-1), (x+1, y-1), (x-1, y), (x+1, y), (x-1, y+1), (x, y+1), (x+1, y+1)]
        validNeighbours = filter (\(x, y) -> x >= 0 && x < width && y >= 0 && y < height) neighbours 
        -- TO DO: cambiar para que los bordes sean ciclicos
    in let idCant = map (\(x, y) -> (cuadr V.! x) V.! y) validNeighbours
       in --trace ("test"++ "\n" ++ 
            --show neighbours ++ "\n" ++ 
            --show validNeighbours ++ "\n" ++ 
            --show idCant ++ "\n" ++ 
            --show cuadr) 
            idCant -- TO DO borrar trace

