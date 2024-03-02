{
    module Parse where
    import Common
}


--%monad { P } { thenP } { returnP }
%name command Com
%name commands Coms

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
    '.'     { TDot }
    '('     { TOpen }
    ')'     { TClose }
    ','     { TComa }
    NVAR    { NVar $$ }
    CVAR    { TColour $$}
    DEF     { TDef }
    INT     { TInt $$ }


Com         : DefCell               { $1 }
            | UpdateC               { $1 }
            | CheckN Position       { CheckN $2 } 
            | Step                  { Step }

DefCell     : DEF NVAR '=' '(' CVAR ',' NList ',' NList ')' { DefCell $1  }

NList       : '[' INT NList             { $2 : $3 }
            | ',' NLIst ']'             { $2 }
            | ']'                      {[]}

UpdateC     : Position NVAR          { UpdateCell $1 $2}

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
        | TDot
        | TOpen
        | TClose
        | TComa
        | TVar String
        | TColour Double
        | TDef
        | TInt Int


    lexer :: String -> [Token]
    lexer [] = []
    lexer (c:cs)
        | isSpace c = lexer cs
        | isAlpha c = lexVar (c:cs)
        | isDigit c = lexNum (c:cs)
    lexer ('=':cs) = TokenEq : lexer cs
    lexer ('+':cs) = TokenPlus : lexer cs
    lexer ('-':cs) = TokenMinus : lexer cs
    lexer ('*':cs) = TokenTimes : lexer cs
    lexer ('/':cs) = TokenDiv : lexer cs
    lexer ('(':cs) = TokenOB : lexer cs
    lexer (')':cs) = TokenCB : lexer cs

    lexNum cs = TInt (read num) : lexer rest
        where (num,rest) = span isDigit cs

    lexVar cs =
    case span isAlpha cs of
        ("def",rest) -> TVar : lexer rest
        --("in",rest)  -> TokenIn : lexer rest
        (var,rest)   -> TokenVar var : lexer rest

    }