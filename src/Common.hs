module Common where
    import Data.Colour 
    import Data.Map.Strict as M


    -- Comandos interactivos o de archivos
    data Stmt e =  Def e
                 | Eval e
        deriving (Show)

    instance Functor Stmt where
        fmap f (UpdateCell p v) = Def s (f i)
        fmap f (Eval i)  = Eval (f i)

    data Cell = M.Map CellId CellData

    type CellId = Int

    type Variable = String

    data CellData = CellData {  id :: CellId,
                                name :: Variable,
                                colour :: Colour Double,
                                bornL :: [Int],
                                surviveL :: [Int] }

    data GridData = GridData {  height :: Int,
                                width :: Int,
                                grid :: [[ (CellId, CellId) ]],
                                upLimit :: Int,
                                loLimit :: Int,
                                leLimit :: Int,
                                riLimit :: Int }

    type Pos = (Int, Int)

    data Comm = UpdateCell Pos Variable 
                | Step 
                | CheckN Pos 
                | Def CellName (Colour Double) [Int] [Int]

    data Error = UndefVar 
        deriving (Eq, Show)
