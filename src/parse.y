{
    module Parse where
    import Common
}


--%monad { P } { thenP } { returnP }
%name command Com

%tokentype { Token }
%lexer {lexer} {TEOF}


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
    ':'     { TColon }
    '\\'    { TAbs }
    '.'     { TDot }
    '('     { TOpen }
    ')'     { TClose }
    '->'    { TArrow }
    ','     { TComa }
    VAR     { TVar $$ }
    CVAR    { TColour $$}
    TYPEE   { TTypeE }
    DEF     { TDef }
    LET     { TLet }
    IN      { TIn }
    UNIT    { TUnitT }
    UNITV   { TUnit }
    FST     { TFst }
    SND     { TSnd }
    ZERO    { TZero }
    SUC     { TSuc }
    NAT     { TNat }
    REC     { TRec }


Com         : DefCell               { $1 }
            | UpdateC               { $1 }
            | CheckN                { CheckN $1 } 
            | Step                  { Step }
DefCell     : VAR '=' '(' CVAR ',' '[' NList ']' ',' '[' NList ']' ')' {}
NList       : NAT NList             { $1 : $2 }
            | ',' NLIst             { $2 }
            |                       {[]}
UpdateC     : Position VAR          { UpdateCell $1 $2}
Position    : '(' NAT ',' NAT ')'   { Pair $2 $4 }
CheckN      : Position              { $1 }
Coms        : Com Coms              { $1 : $2 }
            |                       { [] }


Def     :  Defexp                      { $1 }
        |  Exp	                       { Eval $1 }
Defexp  : DEF VAR '=' Exp              { Def $2 $4 } 

Exp     :: { LamTerm }
        : '\\' VAR ':' Type '.' Exp    { LAbs $2 $4 $6 }
        | LET VAR '=' Exp IN Exp       { LLet $2 $4 $6 }
        | NAbs                         { $1 }
        | FST Exp                      { LFst $2 }
        | SND Exp                      { LSnd $2 }
        | '(' Exp ',' Exp ')'          { LPair $2 $4 }
        | SUC Exp                      { LSuc $2 }
        | REC Atom Atom Exp            { LRec $2 $3 $4 }

NAbs    :: { LamTerm }
        : NAbs Atom                    { LApp $1 $2 }
        | Atom                         { $1 }

Atom    :: { LamTerm }
        : VAR                          { LVar $1 }  
        | '(' Exp ')'                  { $2 }
        | UNITV                        { LUnit }
        | ZERO                         {LZero}  

Type    : TYPEE                        { EmptyT }
        | Type '->' Type               { FunT $1 $3 }
        | '(' Type ')'                 { $2 }
        | UNIT                         { UnitT}
        | '(' Type ',' Type ')'        { PairT $2 $4 }
        | NAT                          { NatT }

Defs    : Defexp Defs                  { $1 : $2 }
        |                              { [] }
     
{
