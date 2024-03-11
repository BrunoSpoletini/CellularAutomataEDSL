{
module Parse where
import Common
import Data.Maybe
import Data.Char
import Data.Tuple
}

%monad { P } { thenP } { returnP }
--%name command Com
--%name commands Coms

%name parseStmt Comm
%name parseStmts Comms

-- %name parseStmtAsk Check
-- %name parseStmtsDefs Comms

%tokentype { Token }
%lexer {lexer} {TEOF}
--%error { parseError }

-- %right VAR

-- %left '=' 
-- %right '->'
-- %right FST SND
-- %right SUC
-- %right REC 
-- %right '\\' '.' LET 
-- %%

-- Limpiar tokens sin uso
%token
    '='     { TEquals }
    '('     { TOpen }
    ')'     { TClose }
    ','     { TComa }
    '['     { TBrackO }
    ']'     { TBrackC }
    NVAR    { NVar $$ }
    --CVAR    { TColour $$}
    DEFCELL     { TDefCell }
    INT     { TInt $$ }
    STEP    { TStep }
    CHECK   { TCheck }
    UPDATE  { TUpdate }

%%

Comms       : Comm Comms            { $1 : $2 }
            |                       { [] }

Comm        : DefCell               { $1 }
            | CHECK Position        { CheckC $2 } 
            | UPDATE Position NVAR  { UpdateCell $2 $3 }
            | STEP                  { Step }

DefCell     : DEFCELL NVAR '=' '(' NVAR ',' '[' NList ']' ',' '[' NList ']' ')' { DefCell $2 $5 $8 $12  }

NList       : INT NList             { $1 : $2 }
            | ',' NList             { $2 }
            |                       {[]}

Position    : '(' INT ',' INT ')'   { ($2, $4) }

lineno      :: { LineNumber }
             : {- empty -}      {% getLineNo }

{
data ParseResult a = Ok a | Failed String
                        deriving Show  

type LineNumber = Int
type P a = String -> LineNumber -> ParseResult a

-- parseError :: Token -> P a
-- parseError = getLineNo `thenP` \line ->
--             failP (show line ++ ": parse error")
    
getLineNo :: P LineNumber
getLineNo = \s l -> Ok l

thenP :: P a -> (a -> P b) -> P b
m `thenP` k = \s l-> case m s l of
                        Ok a     -> k a s l
                        Failed e -> Failed e
                        
returnP :: a -> P a
returnP a = \s l-> Ok a

failP :: String -> P a
failP err = \s l -> Failed err

catchP :: P a -> (String -> P a) -> P a
catchP m k = \s l -> case m s l of
                        Ok a     -> Ok a
                        Failed e -> k e s l

happyError :: P a
happyError = \ s i -> Failed $ "Line "++(show (i::LineNumber))++": Parsing Error\n"++(s)

data Token = 
        TEquals
    | TOpen
    | TClose
    | TComa
    | NVar String
    | TDefCell
    | TInt Int
    | TStep
    | TCheck
    | TUpdate
    | TEOF
    | TBrackO
    | TBrackC

---

lexer cont s = case s of
                [] -> cont TEOF []
                ('\n':s) -> \line -> lexer cont s (line + 1)
                (c:cs)
                    | isSpace c -> lexer cont cs
                    | isAlpha c -> lexVar (c:cs)
                    | isDigit c -> lexNum (c:cs)
                ('=':cs) -> cont TEquals cs
                ('(':cs) -> cont TOpen cs
                (')':cs) -> cont TClose cs
                (',':cs) -> cont TComa cs
                ('[':cs) -> cont TBrackO cs
                (']':cs) -> cont TBrackC cs
                unknown 	-> \line -> Failed $ 
                 "Line "++(show line)++": Cannot be recognized "++(show $ take 10 unknown)++ "..."
                where   lexNum cs = cont (TInt (read num)) rest
                            where (num,rest) = span isDigit cs
                        lexVar cs =
                            case span isAlpha cs of
                                ("DEFCELL", rest) -> cont TDefCell rest
                                ("STEP", rest) -> cont TStep rest
                                ("CHECK", rest) -> cont TCheck rest
                                ("UPDATE", rest) -> cont TUpdate rest
                                (var, rest)   -> cont (NVar var) rest

-- stmtAsk_parse s = parseStmtAsk s 1
-- stmtDefs_parse s = parseStmtsDefs s 1

stmts_parse s = parseStmts s 1
stmt_parse s = parseStmt s 1

}

    -- Gramatica
    -- comm := 'DEFCELL' var '=' '(' var ',' '[' numList ']' ',' '[' numList ']' ')' 
    --       | 'UPDATE' pos var
    --       | 'CHECK' pos
    --       | 'STEP' 

    -- var := letter | letter var

    -- num := digit | digit num

    -- numList :=  num | num numList

    -- letter := 'a'|'b'|...|'z'
    
    -- digit := '0'|'1'|...|'9'