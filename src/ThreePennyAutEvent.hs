
module ThreePennyAutEvent where 


import Control.Monad          (forM_, forM, void)
import Control.Monad.Fix
import Control.Monad.IO.Class

import qualified Control.Monad.Catch             as E 
import qualified Data.Aeson                      as JSON
import qualified Foreign.JavaScript              as JS

import qualified Reactive.Threepenny             as Reactive

-- exports
import Foreign.JavaScript                   (Config(..), defaultConfig)


import Reactive.Threepenny                  hiding (onChange)

import MonadAut
import Common
import Graphics.UI.Threepenny


import           Data.Dynamic                          (Typeable)
import qualified Control.Monad.Trans.RWS.Lazy as Monad (runRWST, RWST)

import System.Exit ( exitWith, ExitCode(ExitFailure) )

--on :: (element -> Event a) -> element -> (a -> UI void) -> UI ()
onAut :: (element -> Event a) -> element -> (a -> Aut void) -> Aut ()
onAut f x = void . onEventAut (f x)
 
--onEvent :: Event a -> (a -> UI void) -> UI (UI ())
onEventAut :: Event a -> (a -> Aut void) -> Aut (Aut ())
onEventAut e h = do
    window <- liftUI askWindow
    -- let flush = liftJSWindow $ \w -> do
    --         mode <- JS.getCallBufferMode w
    --         case mode of
    --             FlushOften -> JS.flushCallBuffer w
    --             _          -> return ()
    unregister <- liftIO $ register e (void . (liftIO . (runUI window . getUIAut) . h))
    return (liftIO unregister)

-- newtype UI a = UI { unUI :: Monad.RWST Window [IO ()] () IO a }
--     deriving (Typeable)

-- -- runUI :: Window -> UI a -> IO a
-- runUIAut :: Window -> Aut a -> IO a
-- runUIAut window m = do
--     (a, _, actions) <- Monad.runRWST (unUI $ getUIAut m) window ()
--     --sequence_ actions
--     return a


getUIAut :: Aut a -> UI a
getUIAut m = do 
    res <- runAut m
    case res of
        (Left err) -> do
            liftIO $ putStrLn "Error"
            liftIO $ exitWith (ExitFailure 1)
        (Right v) -> return v