module Monads where
    
import Common

-- Class for representing monads with state variables
class Monad m => MonadState m where
    -- Looks for the value of a variable
    lookfor :: Variable -> m Int
    -- Changes the value of a variable
    update :: Variable -> Int -> m ()

-- Class for representing monads that can throw errors
class Monad m => MonadError m where
    -- Throws error
    throw :: Error -> m a