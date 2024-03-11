module Monads where
    
import Common

-- Class for representing monads with state variables
class Monad m => MonadState m where

    -- Looks for the information of a cell in the state
    lookforCell :: Ident -> m CellData

    -- Looks for the value of an element in the grid of the state
    checkGrid :: Pos -> m CellId

    -- Updates the state
    updateGrid :: Pos -> CellId -> m ()

    -- Adds a cell to the state
    addCell :: Variable -> Variable -> [Int] -> [Int] -> m ()

-- Class for representing monads that can throw errors
class Monad m => MonadError m where
    -- Throws error
    throw :: Error -> m a