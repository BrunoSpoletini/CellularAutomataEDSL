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

    data Color = Black | Silver | Gray | Grey | White | Maroon | Red | Purple | Fuchsia 
        | Green | Lime | Olive | Yellow | Navy | Blue | Teal | Aqua | Brown | AliceBlue
        deriving (Show, Read, Eq)

    type CellId = Int

    type Variable = String

    data Ident = Id CellId | Var Variable
        deriving (Show, Eq)

    type Grid =  V.Vector ( V.Vector CellId)

    data CellData = CellData {  cId :: CellId,
                                name :: Variable,
                                colour :: Color,
                                bornL :: [Int],
                                surviveL :: [Int] }
        deriving (Show, Eq)
 
    data GridData = GridData {  height :: Int, -- se pueden sacar de la grid
                                width :: Int, -- usando height = V.length cuadr y width = V.length (cuadr V.! 0)
                                grid :: Grid,
                                limits :: [Int],
                                changes :: [Pos]
                                }
        deriving (Show, Eq)

    type Pos = (Int, Int)

    -- Comandos interactivos
    data Comm =   UpdateCell Pos Variable
                | DefCell Variable Variable [Int] [Int]
                | Step 
                | Steps Int
                -- UI Commands
                | Restart Env
                | Select Ident
                | UpdatePos Pos
        deriving (Show)

    data Error = DefaultFileNotFound | InvalidColour | NoCellsDefined | UndefCell | OutOfBounds | NameInUse | ParsingError String
        deriving (Eq, Show)

    type Env = (GridData, ([CellData], CellData))

    -- Tipos de los nombres
    data Name =  Global String
        deriving (Show, Eq)


