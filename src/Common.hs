module Common where
    import Data.Colour 
    --import Data.Map.Strict as M
    import qualified Data.Vector as V
    

    -- -- Comandos interactivos o de archivos
    -- data Stmt e =   Def e |
    --                 Ask e
    --      deriving (Show)


    -- instance Functor Stmt where
    --     fmap f (Def i) = Def (f i)
    --     fmap f (Ask i) = Ask (f i)

    --data Cell = (CellId, CellData)

    type CellId = Int

    type Variable = String

    data Ident = Id CellId | Var Variable

    type Grid =  V.Vector ( V.Vector CellId)

    data CellData = CellData {  cId :: CellId,
                                name :: Variable,
                                colour :: Variable,
                                bornL :: [Int],
                                surviveL :: [Int] }
        deriving (Show)
 
    data GridData = GridData {  height :: Int,
                                width :: Int,
                                grid :: Grid,
                                limits :: [Int]
                                }

    type Pos = (Int, Int)

    data Comm = UpdateCell Pos Variable 
                | Step 
                | CheckC Pos 
                | DefCell Variable Variable [Int] [Int]
        deriving (Show)
        
    data Error = UndefCell | OutOfBounds | NameInUse
        deriving (Eq, Show)

    type Env = (GridData, [CellData])

    data State = State
        {
            cellList :: [CellData],
            gridData :: GridData
        }

      -- Tipos de los nombres
    data Name =  Global  String
        deriving (Show, Eq)

    -- Null enviroment
    initState :: State
    initState = let     deadCell = CellData { cId = 0, 
                                        name = "dead", 
                                        colour = "grey", 
                                        bornL = [], 
                                        surviveL  = [] }
                        grid = GridData { height = 20,
                                        width = 20,
                                        grid = V.fromList (replicate 20 (V.fromList (replicate 20 0))),
                                        limits = [0,0,0,0] }
                    in State { cellList = [deadCell], gridData = grid }

    checkGrid :: Pos -> Grid -> CellId
    checkGrid (x, y) g = (g V.! y) V.! x


    printGrid :: Grid -> String
    printGrid g = V.foldl (\acc x -> acc ++ (V.foldl (\acc y -> acc ++ (show y) ++ " ") "" x) ++ "\n") "" g