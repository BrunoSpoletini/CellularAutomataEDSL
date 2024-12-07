module Config
    where

-- // Constantes de configuracion de la UI //
-- El tamaño de las celulas puede ir de los 5px (100x100 celulas) a los 500px (1x1 celulas)
cellSize = 20 :: Double -- [5 - 500] px

-- En caso de mejorarse la responsiveness de la pagina el tamaño del canvas puede aumentarse
canvasSize = 500 :: Double

-- Path del archivo de salida
outFile = "static/examples/out.txt" :: String
