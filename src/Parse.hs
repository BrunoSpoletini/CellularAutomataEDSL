{-# OPTIONS_GHC -w #-}
module Parse where
import Common
import Data.Maybe
import Data.Char
import Data.Tuple
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.12

data HappyAbsSyn t5 t6 t7 t8 t9
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn5 t5
	| HappyAbsSyn6 t6
	| HappyAbsSyn7 t7
	| HappyAbsSyn8 t8
	| HappyAbsSyn9 t9

happyExpList :: Happy_Data_Array.Array Int Int
happyExpList = Happy_Data_Array.listArray (0,52) ([0,29,1856,53248,1,116,0,8192,0,0,2048,1024,0,0,0,0,2,512,0,8192,0,0,1024,0,4,0,0,8,128,1024,32768,0,0,8192,0,132,1024,16384,8,528,0,0,0,64,8192,0,132,1024,8192,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parseStmt","%start_parseStmts","Comms","Comm","DefCell","NList","Position","'='","'('","')'","','","'['","']'","NVAR","DEFCELL","INT","STEP","STEPS","UPDATE","%eof"]
        bit_start = st * 22
        bit_end = (st + 1) * 22
        read_bit = readArrayBit happyExpList
        bits = map read_bit [bit_start..bit_end - 1]
        bits_indexed = zip bits [0..21]
        token_strs_expected = concatMap f bits_indexed
        f (False, _) = []
        f (True, nr) = [token_strs !! nr]

action_0 (17) = happyShift action_5
action_0 (19) = happyShift action_6
action_0 (20) = happyShift action_7
action_0 (21) = happyShift action_8
action_0 (6) = happyGoto action_10
action_0 (7) = happyGoto action_4
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (17) = happyShift action_5
action_1 (19) = happyShift action_6
action_1 (20) = happyShift action_7
action_1 (21) = happyShift action_8
action_1 (5) = happyGoto action_9
action_1 (6) = happyGoto action_3
action_1 (7) = happyGoto action_4
action_1 _ = happyReduce_3

action_2 (17) = happyShift action_5
action_2 (19) = happyShift action_6
action_2 (20) = happyShift action_7
action_2 (21) = happyShift action_8
action_2 (6) = happyGoto action_3
action_2 (7) = happyGoto action_4
action_2 _ = happyFail (happyExpListPerState 2)

action_3 (17) = happyShift action_5
action_3 (19) = happyShift action_6
action_3 (20) = happyShift action_7
action_3 (21) = happyShift action_8
action_3 (5) = happyGoto action_15
action_3 (6) = happyGoto action_3
action_3 (7) = happyGoto action_4
action_3 _ = happyReduce_3

action_4 _ = happyReduce_4

action_5 (16) = happyShift action_14
action_5 _ = happyFail (happyExpListPerState 5)

action_6 _ = happyReduce_6

action_7 (18) = happyShift action_13
action_7 _ = happyFail (happyExpListPerState 7)

action_8 (11) = happyShift action_12
action_8 (9) = happyGoto action_11
action_8 _ = happyFail (happyExpListPerState 8)

action_9 (22) = happyAccept
action_9 _ = happyFail (happyExpListPerState 9)

action_10 (22) = happyAccept
action_10 _ = happyFail (happyExpListPerState 10)

action_11 (16) = happyShift action_18
action_11 _ = happyFail (happyExpListPerState 11)

action_12 (18) = happyShift action_17
action_12 _ = happyFail (happyExpListPerState 12)

action_13 _ = happyReduce_7

action_14 (10) = happyShift action_16
action_14 _ = happyFail (happyExpListPerState 14)

action_15 _ = happyReduce_2

action_16 (11) = happyShift action_20
action_16 _ = happyFail (happyExpListPerState 16)

action_17 (13) = happyShift action_19
action_17 _ = happyFail (happyExpListPerState 17)

action_18 _ = happyReduce_5

action_19 (18) = happyShift action_22
action_19 _ = happyFail (happyExpListPerState 19)

action_20 (16) = happyShift action_21
action_20 _ = happyFail (happyExpListPerState 20)

action_21 (13) = happyShift action_24
action_21 _ = happyFail (happyExpListPerState 21)

action_22 (12) = happyShift action_23
action_22 _ = happyFail (happyExpListPerState 22)

action_23 _ = happyReduce_12

action_24 (14) = happyShift action_25
action_24 _ = happyFail (happyExpListPerState 24)

action_25 (13) = happyShift action_27
action_25 (18) = happyShift action_28
action_25 (8) = happyGoto action_26
action_25 _ = happyReduce_11

action_26 (15) = happyShift action_31
action_26 _ = happyFail (happyExpListPerState 26)

action_27 (13) = happyShift action_27
action_27 (18) = happyShift action_28
action_27 (8) = happyGoto action_30
action_27 _ = happyReduce_11

action_28 (13) = happyShift action_27
action_28 (18) = happyShift action_28
action_28 (8) = happyGoto action_29
action_28 _ = happyReduce_11

action_29 _ = happyReduce_9

action_30 _ = happyReduce_10

action_31 (13) = happyShift action_32
action_31 _ = happyFail (happyExpListPerState 31)

action_32 (14) = happyShift action_33
action_32 _ = happyFail (happyExpListPerState 32)

action_33 (13) = happyShift action_27
action_33 (18) = happyShift action_28
action_33 (8) = happyGoto action_34
action_33 _ = happyReduce_11

action_34 (15) = happyShift action_35
action_34 _ = happyFail (happyExpListPerState 34)

action_35 (12) = happyShift action_36
action_35 _ = happyFail (happyExpListPerState 35)

action_36 _ = happyReduce_8

happyReduce_2 = happySpecReduce_2  5 happyReduction_2
happyReduction_2 (HappyAbsSyn5  happy_var_2)
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn5
		 (happy_var_1 : happy_var_2
	)
happyReduction_2 _ _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_0  5 happyReduction_3
happyReduction_3  =  HappyAbsSyn5
		 ([]
	)

happyReduce_4 = happySpecReduce_1  6 happyReduction_4
happyReduction_4 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn6
		 (happy_var_1
	)
happyReduction_4 _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_3  6 happyReduction_5
happyReduction_5 (HappyTerminal (NVar happy_var_3))
	(HappyAbsSyn9  happy_var_2)
	_
	 =  HappyAbsSyn6
		 (UpdateCell happy_var_2 happy_var_3
	)
happyReduction_5 _ _ _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_1  6 happyReduction_6
happyReduction_6 _
	 =  HappyAbsSyn6
		 (Step
	)

happyReduce_7 = happySpecReduce_2  6 happyReduction_7
happyReduction_7 (HappyTerminal (TInt happy_var_2))
	_
	 =  HappyAbsSyn6
		 (Steps happy_var_2
	)
happyReduction_7 _ _  = notHappyAtAll 

happyReduce_8 = happyReduce 14 7 happyReduction_8
happyReduction_8 (_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn8  happy_var_12) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn8  happy_var_8) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal (NVar happy_var_5)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal (NVar happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (DefCell happy_var_2 happy_var_5 happy_var_8 happy_var_12
	) `HappyStk` happyRest

happyReduce_9 = happySpecReduce_2  8 happyReduction_9
happyReduction_9 (HappyAbsSyn8  happy_var_2)
	(HappyTerminal (TInt happy_var_1))
	 =  HappyAbsSyn8
		 (happy_var_1 : happy_var_2
	)
happyReduction_9 _ _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_2  8 happyReduction_10
happyReduction_10 (HappyAbsSyn8  happy_var_2)
	_
	 =  HappyAbsSyn8
		 (happy_var_2
	)
happyReduction_10 _ _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_0  8 happyReduction_11
happyReduction_11  =  HappyAbsSyn8
		 ([]
	)

happyReduce_12 = happyReduce 5 9 happyReduction_12
happyReduction_12 (_ `HappyStk`
	(HappyTerminal (TInt happy_var_4)) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TInt happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 ((happy_var_2, happy_var_4)
	) `HappyStk` happyRest

happyNewToken action sts stk
	= lexer(\tk -> 
	let cont i = action i i tk (HappyState action) sts stk in
	case tk of {
	TEOF -> action 22 22 tk (HappyState action) sts stk;
	TEquals -> cont 10;
	TOpen -> cont 11;
	TClose -> cont 12;
	TComa -> cont 13;
	TBrackO -> cont 14;
	TBrackC -> cont 15;
	NVar happy_dollar_dollar -> cont 16;
	TDefCell -> cont 17;
	TInt happy_dollar_dollar -> cont 18;
	TStep -> cont 19;
	TSteps -> cont 20;
	TUpdate -> cont 21;
	_ -> happyError' (tk, [])
	})

happyError_ explist 22 tk = happyError' (tk, explist)
happyError_ explist _ tk = happyError' (tk, explist)

happyThen :: () => P a -> (a -> P b) -> P b
happyThen = (thenP)
happyReturn :: () => a -> P a
happyReturn = (returnP)
happyThen1 :: () => P a -> (a -> P b) -> P b
happyThen1 = happyThen
happyReturn1 :: () => a -> P a
happyReturn1 = happyReturn
happyError' :: () => ((Token), [String]) -> P a
happyError' tk = (\(tokens, explist) -> happyError) tk
parseStmt = happySomeParser where
 happySomeParser = happyThen (happyParse action_0) (\x -> case x of {HappyAbsSyn6 z -> happyReturn z; _other -> notHappyAtAll })

parseStmts = happySomeParser where
 happySomeParser = happyThen (happyParse action_1) (\x -> case x of {HappyAbsSyn5 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


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
happyError = \ s i -> Failed $ "Line "++(show (i::LineNumber))++": Parsing Error\n"

data Token = 
        TEquals
    | TOpen
    | TClose
    | TComa
    | NVar String
    | TDefCell
    | TInt Int
    | TStep
    | TSteps
--    | TCheck
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
                 "Line "++(show line)++": Cannot be recognized "++(show $ take 10 unknown)++ "...\n"
                where   lexNum cs = cont (TInt (read num)) rest
                            where (num,rest) = span isDigit cs
                        lexVar cs =
                            case span isAlpha cs of
                                ("DEFCELL", rest) -> cont TDefCell rest
                                ("STEPS", rest) -> cont TSteps rest
                                ("STEP", rest) -> cont TStep rest
                             --   ("CHECK", rest) -> cont TCheck rest
                                ("UPDATE", rest) -> cont TUpdate rest
                                (var, rest)   -> cont (NVar var) rest

stmts_parse s = parseStmts s 1
stmt_parse s = parseStmt s 1
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- $Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp $










































data Happy_IntList = HappyCons Int Happy_IntList








































infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is ERROR_TOK, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action









































indexShortOffAddr arr off = arr Happy_Data_Array.! off


{-# INLINE happyLt #-}
happyLt x y = (x < y)






readArrayBit arr bit =
    Bits.testBit (indexShortOffAddr arr (bit `div` 16)) (bit `mod` 16)






-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Int ->                    -- token number
         Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k - ((1) :: Int)) sts of
         sts1@(((st1@(HappyState (action))):(_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             _ = nt :: Int
             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction









happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery (ERROR_TOK is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  ERROR_TOK tk old_st CONS(HAPPYSTATE(action),sts) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        DO_ACTION(action,ERROR_TOK,tk,sts,(saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ((HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.









{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
