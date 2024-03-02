{
    module Parse where
    import Common
}


%monad { P } { thenP } { returnP }
%name command Com
--%name commands Coms

%tokentype { Token }
%lexer {lexer} {TEOF}
%error { parseError }

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
    NVAR    { NVar $$ }
    --CVAR    { TColour $$}
    DEF     { TDef }
    INT     { TInt $$ }
    STEP    { TStep }
    CHECK   { TCheck }
    UPDATE  { TUpdate }

Com         : DefCell               { $1 }
            | UPDATE Position NVAR  { UpdateCell $2 $3 }
            | CHECK Position        { CheckN $2 } 
            | STEP                  { Step }

DefCell     : DEF NVAR '=' '(' NVAR ',' '[' NList ']' ',' '[' NList ']' ')' { Def $2 $5 $8 $12  }

NList       : INT NList             { $1 : $2 }
            | ',' NLIst             { $2 }
            |                       {[]}

Position    : '(' INT ',' INT ')'   { Pair $2 $4 }

--Coms        : Com Coms              { $1 : $2 }
--            |                       { [] }

{
    parseError :: [Token] -> a 
    parseError _ = error "Parse error"
        
    data ParseResult a = Ok a | Failed String
                    deriving Show                     
    type LineNumber = Int
    type P a = String -> LineNumber -> ParseResult a

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
    happyError = \ s i -> Failed $ "Línea "++(show (i::LineNumber))++": Error de parseo\n"++(s)

    data Token = 
          TEquals
        | TOpen
        | TClose
        | TComa
        | NVar String
        | TDef
        | TInt Int
        | TStep
        | TCheck
        | TUpdate
        | TEOF

    ---

    lexer cont s = case s of
                    [] -> cont TEOF []
                    ('\n':s) -> \line -> lexer cont s (line + 1)
                    (c:cs)
                        | isSpace c = lexer cont cs
                        | isAlpha c = lexVar (c:cs)
                        | isDigit c = lexNum (c:cs)
                    lexer ('=':cs) = cont TEquals cs
                    lexer ('(':cs) = cont TOpen cs
                    lexer (')':cs) = cont TClose cs
                    lexer (',':cs) = cont TComa cs

                    lexNum cs = cont (TInt (read num)) rest
                        where (num,rest) = span isDigit cs

                    lexVar cs =
                        case span isAlpha cs of
                            ("DEF", rest) -> cont TDef rest
                            ("STEP", rest) -> cont TStep rest
                            ("CHECK", rest) -> cont TCheck rest
                            ("UPDATE", rest) -> cont TUpdate rest
                            (var, rest)   -> cont (NVar var) rest

    --coms_parse s = Coms s 1
    com_parse s = Com s 1

}

    -- Gramatica
    -- comm := 'DEF' var '=' '(' var ',' '[' numList ']' ',' '[' numList ']' ')' 
    --       | 'UPDATE' pos var
    --       | 'CHECK' pos
    --       | 'STEP' 

    -- var := letter | letter var

    -- num := digit | digit num

    -- numList :=  num | num numList

    -- letter := 'a'|'b'|...|'z'
    
    -- digit := '0'|'1'|...|'9'