module Common where
    import qualified Data.Vector as V

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
 
    data GridData = GridData {  grid :: Grid,
                                changes :: [Pos] }
        deriving (Show, Eq)

    type Pos = (Int, Int)

    -- Comandos interactivos
    data Comm =   DefCell Variable Variable [Int] [Int]
                | UpdateCell Pos Variable
                | Step 
                | Steps Int
                -- Comandos de la UI
                | Restart Env
                | Select Ident
                | UpdatePos Pos
        deriving (Show)

    data Error = DefaultFileNotFound | InvalidColour | NoCellsDefined | UndefCell | OutOfBounds | NameInUse | ParsingError String
        deriving (Eq, Show)

    -- Estado del automata, que consiste en la grilla, un array de celulas definidas, y la celula seleccionada
    type Env = (GridData, ([CellData], CellData))

    -- Tipos de los nombres
    data Name =  Global String
        deriving (Show, Eq)


