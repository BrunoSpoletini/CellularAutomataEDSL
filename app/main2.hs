module Main (main) where

import Front
import Parse
import Common
import Automata
import Monads

import           System.Console.GetOpt
import           System.Exit
import           Control.Monad                  ( when )
import qualified System.Environment            as Env
import System.Console.Haskeline

-- VER INPUTT 


main :: IO ()
main = runInputT defaultSettings loop
 
loop :: (MonadState m, MonadError m) => InputT m ()
loop = do
    minput <- getInputLine "> "
    case minput of --usar un tolower ?
        Nothing -> return ()
        Just "quit" -> return ()
        Just input -> do    outputStrLn "TEST" --handleParse input
                            loop

-- handleParse :: String -> IO (Maybe Comm)
-- handleParse s = case stmt_parse s of
--     Failed e -> do  outputStrLn "Error"
--                     return Nothing
--     Ok r -> return (Just r)


-- main :: IO ()
-- main = do   contents <- getContents
--             parsedComm <- handleParse contents
--             case parsedComm of
--                 Nothing -> putStrLn "Parse Error"
--                 Just x -> putStrLn "Test"
--                     --  do    procRes <- processComm x
--                     --             print procRes





--    case stmt_parse "DEFCELL Carla = (rojo, [1,2,3], [6,7,8])\n" of

--
--"DEFCELL Carla = (rojo, [1,2,3], [6,7,8])\n"



---------------------

-- data Options = Options
--     { optPrint :: Bool
--     , optAST   :: Bool
--     , optEval  :: Int
--     , optHelp  :: Bool
--     }
--     deriving Show

-- defaultOptions :: Options
-- defaultOptions =
--   Options { optPrint = False, optAST = False, optEval = 0, optHelp = False }

-- options :: [OptDescr (Options -> Options)]
-- options =
--   [ Option ['p']
--            ["print"]
--            (NoArg (\opts -> opts { optPrint = True }))
--            "Imprimir el programa de entrada."
--   , Option ['a']
--            ["AST"]
--            (NoArg (\opts -> opts { optAST = True }))
--            "Mostrar el AST del programa de entrada."
--   , Option ['e']
--            ["evaluator"]
--            (ReqArg (\s opts -> opts { optEval = read s }) "N_EVALUADOR")
--            "Elegir evaluador 1, 2 o 3."
--   , Option ['h']
--            ["help"]
--            (NoArg (\opts -> opts { optHelp = True }))
--            "Imprimir guia de uso."
--   ]

-- finalOptions :: [String] -> IO (Options, [String])
-- finalOptions argv = case getOpt Permute options argv of
--   (o, n, []  ) -> return (foldl (flip id) defaultOptions o, n)
--   (_, _, errs) -> ioError (userError (concat errs ++ usageInfo header options))
--   where header = "Uso:"

-- main :: IO ()
-- main = do
--   s : opts   <- Env.getArgs
--   (opts', _) <- finalOptions opts
--   runOptions s opts'

-- runOptions :: FilePath -> Options -> IO ()
-- runOptions fp opts
--   | optHelp opts = putStrLn (usageInfo "Uso: " options)
--   | otherwise = do
--     s <- readFile fp
--     case stmt_parse fp s of
--       Left  error -> print error
--       Right ast   -> if
--         | optAST opts       -> print ast
--         | optPrint opts     -> print "Test"--putStrLn (renderComm ast)
--         | optEval opts == 1 -> print (eval)
--         -- | optEval opts == 2 -> print (E2.eval ast)
--         -- | optEval opts == 3 -> print (E3.eval ast)
--         | otherwise         -> print (eval)

-------------------------------------------------------

