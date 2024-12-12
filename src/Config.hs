module Config
    where

-- // Constantes de configuracion de la UI //
-- El tamaño de las celulas puede ir de los 5px (100x100 celulas) a los 500px (1x1 celulas)
-- Debe ser divisible por el canvasSize
cellSize :: Double
cellSize = 20 -- [5 - 500] px

-- En caso de mejorarse la responsiveness de la pagina el tamaño del canvas puede aumentarse
canvasSize :: Double
canvasSize = 500

-- Velocidad a la que se producen los steps
speed :: Int
speed = 400 -- [100 - 1000] ms

-- Path del archivo de salida
outFile :: String
outFile = "./definitions/out.txt"

definitionsPath :: String
definitionsPath = "./definitions/"