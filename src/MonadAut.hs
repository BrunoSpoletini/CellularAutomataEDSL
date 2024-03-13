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

checkCell :: MonadAut m => Pos -> m CellId
checkCell (x, y) = do
    g <- gets gridData
    return $ (grid g V.! y) V.! x

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

-- updateGrid :: MonadAut m => Pos -> CellId -> m ()
-- updateGrid (x, y) idCell = StateError(\s -> 
--     case changeCell idCell (x, y) (fst s) of 
--         Nothing -> Left OutOfBounds
--         Just newGrid -> Right (() :!: (newGrid, snd s)))

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


