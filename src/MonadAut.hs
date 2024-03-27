{-# LANGUAGE UndecidableInstances #-}
module MonadAut where

import Control.Monad.Except --( MonadError(..), ExceptT, runExceptT )
import Control.Monad.State hiding (State)--( MonadIO(..), StateT(runStateT), MonadState(get), gets, modify )
import System.IO --( stderr, hPrint )

import Data.Strict.Tuple hiding (fst, snd)
import qualified Data.Vector as V
import           Graphics.UI.Threepenny.Core hiding (grid, get)

import qualified Control.Monad.Trans.State.Lazy as Lazy
import qualified Control.Monad.Trans.State.Strict as Strict

import Common

-- newtype UI a = UI { unUI :: Monad.RWST Window [IO ()] () IO a }
--     deriving (Typeable)

-- class (Monad m) => MonadUI m where
--     -- | Lift a computation from the 'UI' monad.
--     liftUI :: UI a -> m a

-- instance MonadUI UI where
--     liftUI = id

instance (Monad m, MonadUI m) => MonadUI (Lazy.StateT State m) where
    liftUI = lift . liftUI

instance (Monad m, MonadUI m) => MonadUI (Strict.StateT State m) where
    liftUI = lift . liftUI

instance (Monad m, MonadUI m) => MonadUI (ExceptT Error m) where
    liftUI = lift . liftUI

class (MonadUI m, MonadState State m, MonadError Error m) => MonadAut m where

getGrid :: MonadAut m => m Grid
getGrid = gets (grid . gridData)

setGrid :: MonadAut m => Grid -> m ()
setGrid g = modify (\s-> s { gridData = (gridData s) { grid = g }})

setState :: MonadAut m => State -> m ()
setState s = modify (\_ -> s)

checkCell :: MonadAut m => Pos -> m CellId
checkCell (x, y) = do
    g <- gets gridData
    return $ (grid g V.! y) V.! x


initState2 :: Int -> State
initState2 n=let        deadCell = CellData { cId = 0, 
                                        name = "dead", 
                                        colour = "grey", 
                                        bornL = [], 
                                        surviveL  = [] }
                        blackCell = CellData { cId = 1, 
                                        name = "black", 
                                        colour = "black", 
                                        bornL = [], 
                                        surviveL  = [] }                
                        grid = GridData { height = 20,
                                        width = 20,
                                        grid = V.fromList (replicate 20 (V.fromList (replicate 20 n))),
                                        limits = [0,0,0,0] }
                in State { cellList = [deadCell, blackCell], gridData = grid }




updateCell :: MonadAut m => Pos -> CellId -> m ()
updateCell pos idCell = do  g <- gets gridData
                            case changeCell idCell pos g of
                                Nothing -> throwError OutOfBounds
                                Just newGridData -> modify (\s-> s { gridData = newGridData })

-- changeCell :: CellId -> Pos -> GridData -> Maybe GridData

-- -- -- addCell :: MonadAut m => Variable -> Variable -> [Int] -> [Int] -> m ()
-- -- -- addCell var col xs ys = do
-- -- --     s <- get
-- -- --     case searchCellName s var of
-- -- --         Nothing -> do
-- -- --             let cell = createCell s var col xs ys
-- -- --             put (s { cellList = cell : cellList s })
-- -- --         Just _ -> throwError NameInUse

-- -- -- createCell :: State -> Variable -> Variable -> [Int] -> [Int] -> CellData
-- -- -- createCell s var col xs ys = CellData { cId = length (cellList s),
-- -- --                                         name = var,
-- -- --                                         colour = col,
-- -- --                                         bornL = xs,
-- -- --                                         surviveL = ys }

-- lookforCell :: MonadAut m => Ident -> m CellData 
-- lookforCell ident = StateError (\s -> 
--     case ident of
--         Var name -> (case searchCellName s name of
--                         Nothing -> Left UndefCell
--                         Just cellData -> Right (cellData :!: s))
--         Id id -> case searchCellId s id of
--                     Nothing -> Left UndefCell
--                     Just cellData -> Right (cellData :!: s) )

-- checkGrid :: MonadAut m => Pos -> m CellId 
-- checkGrid (x, y) = StateError(\s -> Right (((grid (fst s) V.! y) V.! x):!: s) )


-- addCell :: MonadAut m => Variable -> Variable -> [Int] -> [Int] -> m ()            
-- addCell var col xs ys = StateError(\s -> 
--     case runStateError (lookforCell (Var var)) s of
--         Left UndefCell -> Right (() :!: (fst s, cell : snd s)) 
--                             where cell = createCell (snd s) var col xs ys
--         Right x -> Left NameInUse)

type Aut = StateT State (ExceptT Error UI)

instance MonadAut Aut

runAut' :: Aut a -> UI (Either Error (a, State))
runAut' m = do
  runExceptT $ runStateT m initState

runAut :: Aut a -> UI (Either Error a)
runAut m = fmap fst <$> runAut' m


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