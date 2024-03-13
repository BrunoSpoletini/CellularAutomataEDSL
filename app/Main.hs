

module Main where

import           Control.Exception              ( catch
                                                , IOException
                                                )
import           Control.Monad.Except
import           Data.Char
import           Data.List
import           Data.Maybe
import           Prelude                 hiding ( print )
import           System.Console.Haskeline
import qualified Control.Monad.Catch           as MC
import           System.Environment
import           System.IO               hiding ( print )
-- import           Text.PrettyPrint.HughesPJ      ( render
--                                                 , text
--                                                 )
import Data.IORef

import           Graphics.UI.Threepenny      as UI hiding (map)
import           Graphics.UI.Threepenny.Canvas as Canvas
import           Graphics.UI.Threepenny.Core

import           Front
import           Common
import           Parse
import           Automata

import           Control.Parallel

---------------------
--- Interpreter
---------------------



main :: IO ()
--main = startCA
main = runInputT defaultSettings main'

startCA :: IO ()
--startCA :: IO ()
startCA = startGUI defaultConfig { jsStatic = Just "."} setup --, jsLog = "Test" 

setup :: Window -> UI ()
setup window = do setupFront window



-- addFun window = do  canvas <- getElem window "canvas" -- no funciona, cada get te recarga la pagina
--                     on UI.mousedown canvas $ \(x, y) -> do
--                         posC <- getIndex canvas x y cellSize
--                         deb <- getElem window "debug"
--                         element deb # set text ("DEBUG: " ++ show posC)
--                         drawSquare canvas x y cellSize "Red"

-- setupEnv :: Window -> UI ()
-- setupEnv window = do
--     -- store env value in an IORef
--     env <- liftIO $ newIORef initEnv
--     return ()




main' :: InputT IO ()
main' = do
  args <- lift getArgs
  readevalprint args (S True "" initEnv)

iname, iprompt :: String
iname = "Autómatas Celulares"
iprompt = "ST> "

ioExceptionCatcher :: IOException -> IO (Maybe a)
ioExceptionCatcher _ = return Nothing

data State = S
  { inter :: Bool
  ,       -- True, if we are in interactive mode
    lfile :: String
  ,     -- Last-loaded file (para hacer "reload")
    env    :: Env 
    -- Enviroment with the data of the grid and an array of cells  
    -- (GridData, [CellData])
  }

--  read-eval-print loop
readevalprint :: [String] -> State -> InputT IO ()
readevalprint args state@(S inter lfile enviroment) =
  let rec st = do
        mx <- MC.catch
          (if inter then getInputLine iprompt else lift $ fmap Just getLine)
          (lift . ioExceptionCatcher)
        lift $ putStrLn ""
        case mx of
          Nothing -> return ()
          Just "" -> rec st
          Just x  -> do
            c   <- interpretCommand x
            st' <- handleCommand st c
            maybe (return ()) rec st'
  in  do
        state' <- compileFiles (prelude : args) state
        liftIO $ startCA (env state')
        -- when inter $ lift $ putStrLn
        --   (  "Intérprete de "
        --   ++ iname
        --   ++ ".\n"
        --   ++ "Escriba :? para recibir ayuda."
        --   )
        -- --  enter loop
        -- rec state' { inter = True }

data Command = Compile CompileForm
              | Print String
              | Recompile
              | Browse
              | Quit
              | Help
              | Noop
              | FindType String

data CompileForm = CompileInteractive  String
                  | CompileFile         String

interpretCommand :: String -> InputT IO Command
interpretCommand x = lift $ if isPrefixOf ":" x
  then do
    let (cmd, t') = break isSpace x
    let t         = dropWhile isSpace t'
    --  find matching commands
    let matching = filter (\(Cmd cs _ _ _) -> any (isPrefixOf cmd) cs) commands
    case matching of
      [] -> do
        putStrLn
          ("Comando desconocido `" ++ cmd ++ "'. Escriba :? para recibir ayuda."
          )
        return Noop
      [Cmd _ _ f _] -> do
        return (f t)
      _ -> do
        putStrLn
          (  "Comando ambigüo, podría ser "
          ++ concat (intersperse ", " [ head cs | Cmd cs _ _ _ <- matching ])
          ++ "."
          )
        return Noop
  else return (Compile (CompileInteractive x))

handleCommand :: State -> Command -> InputT IO (Maybe State)
handleCommand state@(S inter lfile env) cmd = case cmd of
  Quit   -> lift $ when (not inter) (putStrLn "!@#$^&*") >> return Nothing
  Noop   -> return (Just state)
  Help   -> lift $ putStr (helpTxt commands) >> return (Just state)
--   Browse -> lift $ do
--     putStr (unlines [ s | Global s <- reverse (nub (map fst env)) ])
--     return (Just state)
  Compile c -> do
    state' <- case c of
      CompileInteractive s -> compilePhrase state s
      CompileFile        f -> compileFile (state { lfile = f }) f
    return (Just state')
--   Print s ->
--     let s' = reverse (dropWhile isSpace (reverse (dropWhile isSpace s)))
--     in  printPhrase s' >> return (Just state)
--   Recompile -> if null lfile
--     then lift $ putStrLn "No hay un archivo cargado.\n" >> return (Just state)
--     else handleCommand state (Compile (CompileFile lfile))
--   FindType s -> do
--     x' <- parseIO "<interactive>" term_parse s
--     t  <- case x' of
--       Nothing -> return $ Left "Error en el parsing."
--       Just x  -> return $ infer env $ conversion $ x
--     case t of
--       Left  err -> lift (putStrLn ("Error de tipos: " ++ err)) >> return ()
--       Right t'  -> lift $ putStrLn $ render $ printType t'
--     return (Just state)

data InteractiveCommand = Cmd [String] String (String -> Command) String

commands :: [InteractiveCommand]
commands =
  [ Cmd [":browse"] "" (const Browse) "Ver los nombres en scope"
  , Cmd [":load"]
        "<file>"
        (Compile . CompileFile)
        "Cargar un programa desde un archivo"
  , Cmd [":print"] "<exp>" Print "Imprime un término y sus ASTs"
  , Cmd [":reload"]
        "<file>"
        (const Recompile)
        "Volver a cargar el último archivo"
  , Cmd [":quit"]       ""       (const Quit) "Salir del intérprete"
  , Cmd [":help", ":?"] ""       (const Help) "Mostrar esta lista de comandos"
  , Cmd [":type"]       "<term>" (FindType)   "Inferir el tipo de un término"
  ]

helpTxt :: [InteractiveCommand] -> String
helpTxt cs =
  "Lista de comandos:  Cualquier comando puede ser abreviado a :c donde\n"
    ++ "c es el primer caracter del nombre completo.\n\n"
    ++ "<expr>                  evaluar la expresión\n"
    ++ "def <var> = <expr>      definir una variable\n"
    ++ unlines
         (map
           (\(Cmd c a _ d) ->
             let
               ct =
                 concat
                   (intersperse ", "
                                (map (++ if null a then "" else " " ++ a) c)
                   )
             in  ct ++ replicate ((24 - length ct) `max` 2) ' ' ++ d
           )
           cs
         )

compileFiles :: [String] -> State -> InputT IO State
compileFiles xs s =
  foldM (\s x -> compileFile (s { lfile = x, inter = False }) x) s xs

compileFile :: State -> String -> InputT IO State
compileFile state@(S inter lfile v) f = do
  lift $ putStrLn ("Abriendo " ++ f ++ "...")
  let f' = reverse (dropWhile isSpace (reverse f))
  x <- lift $ Control.Exception.catch
    (readFile f')
    (\e -> do
      let err = show (e :: IOException)
      hPutStr stderr
              ("No se pudo abrir el archivo " ++ f' ++ ": " ++ err ++ "\n")
      return ""
    )
  stmts <- parseIO f' (stmts_parse) x
  maybe (return state) (foldM handleStmt state) stmts


compilePhrase :: State -> String -> InputT IO State
compilePhrase state x = do
  x' <- parseIO "<interactive>" stmt_parse x
  maybe (return state) (handleStmt state) x'

printArr x = "test"

-- printStmt :: Comm -> InputT IO ()
-- printStmt stmt = lift $ do
--     let outtext = case stmt of
--             UpdateCell (x, y) var -> "Update " ++ "(" ++ x ++ "," ++ y ++ ") " ++ var ++ "\n"
--             Step -> "Step\n"
--             CheckC (x, y) -> "Check (" ++ x ", " y ")\n"
--             DefCell var colour xs ys -> "DeffCell " ++ var ++ "= (" ++ colour ++ " , " ++ printArr xs ++ ", " ++ printArr ys ++ ")\n"
--     putStrLn outtext

parseIO :: String -> (String -> ParseResult a) -> String -> InputT IO (Maybe a)
parseIO f p x = lift $ case p x of
  Failed e -> do
    putStrLn (f ++ ": " ++ e)
    return Nothing
  Ok r -> return (Just r)

handleStmt :: State -> Comm -> InputT IO State
handleStmt state@(S inter lfile env) stmt = lift $ do
  case stmt of
    CheckC (x, y) -> if inter then putStrLn (checkCell (x, y) env) >> return state
                        else return state
    _ -> case eval stmt env of
            Left err -> putStrLn (show err) >> return state
            Right nEnv -> return $ S inter lfile nEnv

prelude :: String
prelude = "Ejemplos/testing.txt"

it :: String
it = "it"

