module Main where
    
import           Data.List
import           Data.Maybe
import           Prelude                 hiding ( print )

import Data.Strict.Tuple hiding (fst, snd)
import System.Directory (getDirectoryContents)
import           Graphics.UI.Threepenny      as UI hiding (map, grid, drop)

import           Front
import           Common
import           Parse
import           Automata
import           Config   

main :: IO ()
main = do   res <- compileFiles
            case res of
                Left err -> errorHandler err
                Right (envsP, commsL) -> startCA envsP commsL

-- Inicia la GUI
startCA :: [(String, Env)] -> [[Comm]] -> IO ()
startCA envs commsL = do
    startGUI defaultConfig { jsPort = Just 8023, jsStatic = Just "static"} (setup envs commsL)

setup :: [(String, Env)] -> [[Comm]] -> Window -> UI ()
setup envs commsL window = setupFront window envs commsL

-- Parsea un archivo, corre la monada y devuelve el entorno o un error
compileFile :: String -> IO (Either Error (Env, [Comm]))
compileFile file = do
  putStrLn ("Abriendo " ++ file ++ "...")
  x <- readFile file
  case stmts_parse x of
    Failed e -> return $ Left (ParsingError e)
    Ok stmts -> let stmtsS = stmts  in
                case runStateError (loadMonad stmtsS) initEnv of
                    Left err -> return $ Left err
                    Right (_ :!: s) -> return $ Right (s, stmtsS)

-- Carga los archivos de la carpeta ejemplos o devuelve un error
compileFiles :: IO (Either Error ([(String, Env)], [[Comm]]))
compileFiles = do
    files <- drop 2 <$> getDirectoryContents definitionsPath
    let def = filter ("default.txt" == ) files
    if def == [] then
        return $ Left DefaultFileNotFound
    else do
        let orderedFiles = def ++ (Data.List.delete "default.txt" files)
        comp orderedFiles where
        comp :: [String] -> IO (Either Error ([(String, Env)], [[Comm]]))
        comp [] = return (Right ([], []))
        comp (f:fs) = do
            res <- compileFile (definitionsPath ++ f)
            case res of
                Left err -> return (Left err)
                Right (env, comms) -> do
                    res' <- comp fs
                    case res' of
                        Left err -> return (Left err)
                        Right (envsP, commsL) -> return (Right ((f, env):envsP , comms:commsL))

errorHandler :: Error -> IO ()
errorHandler err = do 
    putStr "Error: "
    case err of
        DefaultFileNotFound -> putStrLn "No se encontró el archivo default.txt"
        UndefCell -> putStrLn "Celula no definida"
        OutOfBounds -> putStrLn "Fuera de los límites de la grilla"
        NameInUse -> putStrLn "El nombre ya está en uso"
        NoCellsDefined -> putStrLn "No hay celulas definidas"
        InvalidColour -> putStrLn "Color inválido"
        ParsingError e -> putStrLn e