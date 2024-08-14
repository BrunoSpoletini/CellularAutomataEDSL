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
        deriving (Show, Eq)
 
    data GridData = GridData {  height :: Int,
                                width :: Int,
                                grid :: Grid,
                                limits :: [Int]
                                }
        deriving (Show)

    type Pos = (Int, Int)

    data Comm = UpdateCell Pos Variable 
                | UpdatePos Pos
                | Step 
                | CheckC Pos --pending
                | DefCell Variable Variable [Int] [Int]
                | Restart Env -- UI shortcut
                | Select CellId
        deriving (Show)

    data Error = UndefCell | OutOfBounds | NameInUse
        deriving (Eq, Show)

    type Env = (GridData, ([CellData], CellData))

      -- Tipos de los nombres
    data Name =  Global  String
        deriving (Show, Eq)


