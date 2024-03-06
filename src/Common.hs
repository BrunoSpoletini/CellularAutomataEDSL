module Common where
    import Data.Colour 
    --import Data.Map.Strict as M
    import qualified Data.Vector as V

    -- Comandos interactivos o de archivos
    data Stmt e =  Com e
         deriving (Show)
    --              | Eval e

    instance Functor Stmt where
        fmap f (Com i) = Com (f i)
    --     fmap f (Eval i)  = Eval (f i)

    --data Cell = (CellId, CellData)

    type CellId = Int

    type Variable = String

    data CellData = CellData {  id :: CellId,
                                name :: Variable,
                                colour :: Colour Double,
                                bornL :: [Int],
                                surviveL :: [Int] }

    data GridData = GridData {  height :: Int,
                                width :: Int,
                                grid :: V.Vector ( V.Vector (CellId, CellId)),
                                upLimit :: Int,
                                loLimit :: Int,
                                leLimit :: Int,
                                riLimit :: Int }

    type Pos = (Int, Int)

    data Comm = UpdateCell Pos Variable 
                | Step 
                | CheckN Pos 
                | DefCell Variable Variable [Int] [Int]
        deriving (Show)
        
    data Error = UndefVar 
        deriving (Eq, Show)

    type Env = (GridData, [CellData])