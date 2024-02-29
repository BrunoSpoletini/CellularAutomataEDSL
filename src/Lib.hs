module Lib
    ( startCA
    ) where

import Data.IORef

import qualified Graphics.UI.Threepenny      as UI
import           Graphics.UI.Threepenny.Core


startCA :: IO ()
startCA = startGUI defaultConfig { jsStatic = Just "." } setup

setup :: Window -> UI ()
setup window = do
    UI.addStyleSheet window "foundation-5.css"
    UI.addStyleSheet window "grid.css"
    return  window # set UI.title "Cellular Automata"
    getBody window #+ [ mkDisplay ]
    return ()

n = 5

createGrid :: Int -> [[UI Element]]
createGrid n = replicate n (replicate n ( UI.div #. "cell" ))

-- | Create counter element
mkDisplay :: UI Element
mkDisplay = do
    grid    <- grid (createGrid n) #. "grid"
    --testButton     <- UI.button # set UI.text "Testeo"
    --bup     <- UI.button # set UI.text "Up"
    --bdown   <- UI.button # set UI.text "Down"
    --counter <- UI.div    # set UI.text "0"

    -- -- store counter value in an IORef
    -- count <- liftIO $ newIORef 0
    
    -- -- join the two click events into a single event
    -- let eclick :: Event (Int -> Int)
    --     eclick = unionWith const
    --         ( (+1)         <$ UI.click bup   )
    --         ( (subtract 1) <$ UI.click bdown )

    -- -- handle click
    -- onEvent eclick $ \f -> do
    --     x <- liftIO $ do
    --         modifyIORef count f
    --         readIORef count
    --     element counter # set UI.text (show x)

    -- -- visual style
    -- UI.div #. "row" #+
    --     [ UI.ul #. "button-group round" #+
    --         map (\x -> UI.li #+ [element x]) [bup, bdown]
    --     , element counter #. "small-2 columns panel" 
    --     ] #+
    --     [element grid] #+
    --     [element testButton]
    

    UI.div #. "grid-container" #+
        [
            UI.div #. "header",
            UI.div #. "menu",
            UI.div #. "main"#+
                [element grid],

            UI.div #. "right",
            UI.div #. "footer"

        ]


    
