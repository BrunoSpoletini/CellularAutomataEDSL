module Automata where

import Common
import Monads
import Prelude
-- import qualified Data.Map.Strict as M
import Data.Strict.Tuple

-- Va a tener una version de lo siguiente: EN DESARROLLO

-- Enviroments
-- type Env = (GridData, [CellData]) -- declared in common


deadCell = CellData {   cId = 0, 
                        name = "dead", 
                        colour = "grey", 
                        bornL = [], 
                        surviveL  = [] }

grid = GridData {   height = 100,
                    width = 100,
                    grid = fromList (replicate 100 (fromList (replicate 100 0))),
                    limits = [0,0,0,0] }

-- Null enviroment
initEnv :: Env
initEnv = (grid, [deadCell])

-- State Monad with Error Handler
newtype StateError a =
  StateError { runStateError :: Env -> Either Error ( Pair a Env) }

-- Para calmar al GHC
instance Functor StateError where
  fmap = liftM

instance Applicative StateError where
  pure  = return
  (<*>) = ap

instance Monad StateError where
  return x = StateError (\s -> Right (x :!: s))
  m >>= f = StateError (\s ->   let e = runStateError m s 
                                in case e of
                                    (Left err) -> Left err
                                    (Right (v :!: s')) -> runStateError (f v) s')

-- Ejercicio 2.b: Dar una instancia de MonadError para StateError:
instance MonadError StateError where
  throw e = StateError(\s -> Left e)

-- Ejercicio 2.c: Dar una instancia de MonadState para StateError:
instance MonadState StateError where
  lookfor v = StateError (\s -> lookfor' v s)
    where lookfor' v s = case M.lookup v s of
                          Nothing -> Left UndefVar
                          (Just x) -> Right (x :!: s)
  update v i = StateError (\s -> Right (() :!: (update' v i s))) 
    where update' = M.insert

-- Ejercicio 2.d: Implementar el evaluador utilizando la monada StateError.
-- Evalua un programa en el estado nulo
eval :: Comm -> Either Error Env
eval c =  case runStateError (stepCommStar c) initEnv of
            (Left err) -> Left err
            (Right (v :!: s')) -> Right s'
