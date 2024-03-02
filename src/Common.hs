module Common where
    import Data.Colour 
    import Data.Map.Strict as M

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

    data Action = UpdateCell Pos CellId 
                | Step 
                | CheckN Pos 

    -- CheckN deberia ir ahi?

    data Error = UndefVar 
        deriving (Eq, Show)
