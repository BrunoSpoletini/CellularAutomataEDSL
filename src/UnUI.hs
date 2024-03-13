module UnUI (unUI) where
    import qualified Control.Monad.Trans.RWS.Lazy as Monad (runRWST, RWST)
    import Graphics.UI.Threepenny

    newtype UI a = UI { unUI :: Monad.RWST Window [IO ()] () IO a }
    
 