{-# OPTIONS_GHC -w #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module Samoyeet.Par where
import Samoyeet.Abs
import Samoyeet.Lex
import Samoyeet.ErrM
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.12

data HappyAbsSyn 
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 (Ident)
	| HappyAbsSyn5 (Integer)
	| HappyAbsSyn6 (String)
	| HappyAbsSyn7 (Program)
	| HappyAbsSyn8 (Arg)
	| HappyAbsSyn9 ([Arg])
	| HappyAbsSyn10 (Block)
	| HappyAbsSyn11 ([Stmt])
	| HappyAbsSyn12 (Stmt)
	| HappyAbsSyn13 (Item)
	| HappyAbsSyn14 ([Item])
	| HappyAbsSyn15 (SType)
	| HappyAbsSyn16 (MaybeRefType)
	| HappyAbsSyn17 ([MaybeRefType])
	| HappyAbsSyn18 ([SType])
	| HappyAbsSyn19 (Expr)
	| HappyAbsSyn26 ([Expr])
	| HappyAbsSyn27 (AddOp)
	| HappyAbsSyn28 (MulOp)
	| HappyAbsSyn29 (RelOp)

{- to allow type-synonyms as our monads (likely
 - with explicitly-specified bind and return)
 - in Haskell98, it seems that with
 - /type M a = .../, then /(HappyReduction M)/
 - is not allowed.  But Happy is a
 - code-generator that can just substitute it.
type HappyReduction m = 
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> m HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> m HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> m HappyAbsSyn
-}

action_0,
 action_1,
 action_2,
 action_3,
 action_4,
 action_5,
 action_6,
 action_7,
 action_8,
 action_9,
 action_10,
 action_11,
 action_12,
 action_13,
 action_14,
 action_15,
 action_16,
 action_17,
 action_18,
 action_19,
 action_20,
 action_21,
 action_22,
 action_23,
 action_24,
 action_25,
 action_26,
 action_27,
 action_28,
 action_29,
 action_30,
 action_31,
 action_32,
 action_33,
 action_34,
 action_35,
 action_36,
 action_37,
 action_38,
 action_39,
 action_40,
 action_41,
 action_42,
 action_43,
 action_44,
 action_45,
 action_46,
 action_47,
 action_48,
 action_49,
 action_50,
 action_51,
 action_52,
 action_53,
 action_54,
 action_55,
 action_56,
 action_57,
 action_58,
 action_59,
 action_60,
 action_61,
 action_62,
 action_63,
 action_64,
 action_65,
 action_66,
 action_67,
 action_68,
 action_69,
 action_70,
 action_71,
 action_72,
 action_73,
 action_74,
 action_75,
 action_76,
 action_77,
 action_78,
 action_79,
 action_80,
 action_81,
 action_82,
 action_83,
 action_84,
 action_85,
 action_86,
 action_87,
 action_88,
 action_89,
 action_90,
 action_91,
 action_92,
 action_93,
 action_94,
 action_95,
 action_96,
 action_97,
 action_98,
 action_99,
 action_100,
 action_101,
 action_102,
 action_103,
 action_104,
 action_105,
 action_106,
 action_107,
 action_108,
 action_109,
 action_110,
 action_111,
 action_112,
 action_113,
 action_114,
 action_115,
 action_116,
 action_117,
 action_118,
 action_119,
 action_120,
 action_121,
 action_122,
 action_123,
 action_124,
 action_125,
 action_126,
 action_127,
 action_128,
 action_129,
 action_130,
 action_131,
 action_132 :: () => Int -> ({-HappyReduction (Err) = -}
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Err) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Err) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (Err) HappyAbsSyn)

happyReduce_1,
 happyReduce_2,
 happyReduce_3,
 happyReduce_4,
 happyReduce_5,
 happyReduce_6,
 happyReduce_7,
 happyReduce_8,
 happyReduce_9,
 happyReduce_10,
 happyReduce_11,
 happyReduce_12,
 happyReduce_13,
 happyReduce_14,
 happyReduce_15,
 happyReduce_16,
 happyReduce_17,
 happyReduce_18,
 happyReduce_19,
 happyReduce_20,
 happyReduce_21,
 happyReduce_22,
 happyReduce_23,
 happyReduce_24,
 happyReduce_25,
 happyReduce_26,
 happyReduce_27,
 happyReduce_28,
 happyReduce_29,
 happyReduce_30,
 happyReduce_31,
 happyReduce_32,
 happyReduce_33,
 happyReduce_34,
 happyReduce_35,
 happyReduce_36,
 happyReduce_37,
 happyReduce_38,
 happyReduce_39,
 happyReduce_40,
 happyReduce_41,
 happyReduce_42,
 happyReduce_43,
 happyReduce_44,
 happyReduce_45,
 happyReduce_46,
 happyReduce_47,
 happyReduce_48,
 happyReduce_49,
 happyReduce_50,
 happyReduce_51,
 happyReduce_52,
 happyReduce_53,
 happyReduce_54,
 happyReduce_55,
 happyReduce_56,
 happyReduce_57,
 happyReduce_58,
 happyReduce_59,
 happyReduce_60,
 happyReduce_61,
 happyReduce_62,
 happyReduce_63,
 happyReduce_64,
 happyReduce_65,
 happyReduce_66,
 happyReduce_67,
 happyReduce_68,
 happyReduce_69,
 happyReduce_70,
 happyReduce_71,
 happyReduce_72,
 happyReduce_73,
 happyReduce_74,
 happyReduce_75,
 happyReduce_76,
 happyReduce_77,
 happyReduce_78,
 happyReduce_79,
 happyReduce_80 :: () => ({-HappyReduction (Err) = -}
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Err) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Err) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (Err) HappyAbsSyn)

happyExpList :: Happy_Data_Array.Array Int Int
happyExpList = Happy_Data_Array.listArray (0,412) ([0,0,0,0,0,0,0,0,16384,0,0,0,0,0,0,0,0,0,0,0,34850,61120,3838,0,16384,2084,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,32768,0,0,0,0,0,0,0,0,0,0,0,16656,0,0,0,0,36,0,0,0,32,2520,0,0,0,0,4096,0,0,0,16,0,0,0,8,2048,898,0,4352,16388,1040,7,0,32,8192,3592,0,0,0,0,0,0,0,4,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8192,0,0,0,0,0,0,0,0,34816,544,8322,56,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8,0,0,0,4352,16388,1040,7,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4096,1089,63350,127,0,512,0,0,0,0,4096,0,0,0,2176,8194,33288,3,0,16384,0,0,0,0,0,0,0,0,4164,16640,7184,0,0,0,21001,0,0,0,4608,164,0,0,0,0,0,0,2048,0,0,0,0,0,0,0,0,0,0,0,0,0,2082,8320,3592,0,17408,16,4160,28,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8704,8,2080,14,0,4164,16384,7184,0,0,0,0,0,0,0,0,0,0,8192,130,33280,224,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,32,4,0,0,0,8,0,0,0,0,2,0,0,4096,65,16644,112,0,0,8,0,0,0,4096,0,0,0,2176,8194,33288,3,0,16384,0,0,0,0,0,0,0,0,0,0,0,0,0,16,0,0,0,512,0,0,0,0,0,0,0,0,0,0,16384,0,0,0,8336,5,0,4352,16388,1040,7,0,0,0,0,0,4096,65,0,0,0,0,0,0,0,0,72,0,0,0,0,0,0,0,0,0,0,0,0,2048,0,0,0,0,512,0,0,0,16384,0,0,0,0,32,0,1024,0,0,1,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,0,0,0,0,0,4352,24644,32631,7,0,34850,61120,3838,0,0,0,0,0,0,0,0,2048,0,0,512,0,0,0,0,9216,328,0,0,0,36936,2,0,0,0,0,0,0,32,0,0,0,0,1024,0,0,0,0,0,0,0,0,0,0,0,0,16656,1024,28737,0,0,0,0,0,0,0,0,0,0,0,0,0,32,0,2048,0,0,0,0,1024,0,0,0,32768,0,0,0,0,0,0,0,0,0,0,42002,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,17425,30560,1919,0,0,0,32768,0,0,0,32,0,0,0,0,21001,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_pProgram","Ident","Integer","String","Program","Arg","ListArg","Block","ListStmt","Stmt","Item","ListItem","SType","MaybeRefType","ListMaybeRefType","ListSType","Expr6","Expr5","Expr4","Expr3","Expr2","Expr1","Expr","ListExpr","AddOp","MulOp","RelOp","'!'","'!='","'%'","'&'","'('","')'","'*'","'+'","'++'","','","'-'","'--'","'/'","':'","';'","'<'","'<='","'='","'=='","'>'","'>='","'Fun'","'[]'","'and'","'boolean'","'break;'","'continue;'","'else'","'false'","'if'","'int'","'or'","'return'","'string'","'true'","'void'","'while'","'yeet'","'{'","'}'","L_ident","L_integ","L_quoted","%eof"]
        bit_start = st * 73
        bit_end = (st + 1) * 73
        read_bit = readArrayBit happyExpList
        bits = map read_bit [bit_start..bit_end - 1]
        bits_indexed = zip bits [0..72]
        token_strs_expected = concatMap f bits_indexed
        f (False, _) = []
        f (True, nr) = [token_strs !! nr]

action_0 (7) = happyGoto action_3
action_0 (11) = happyGoto action_4
action_0 _ = happyReduce_11

action_1 (70) = happyShift action_2
action_1 _ = happyFail (happyExpListPerState 1)

action_2 _ = happyReduce_1

action_3 (73) = happyAccept
action_3 _ = happyFail (happyExpListPerState 3)

action_4 (30) = happyShift action_18
action_4 (34) = happyShift action_19
action_4 (40) = happyShift action_20
action_4 (44) = happyShift action_21
action_4 (51) = happyShift action_22
action_4 (52) = happyShift action_23
action_4 (54) = happyShift action_24
action_4 (55) = happyShift action_25
action_4 (56) = happyShift action_26
action_4 (58) = happyShift action_27
action_4 (59) = happyShift action_28
action_4 (60) = happyShift action_29
action_4 (62) = happyShift action_30
action_4 (63) = happyShift action_31
action_4 (64) = happyShift action_32
action_4 (65) = happyShift action_33
action_4 (66) = happyShift action_34
action_4 (67) = happyShift action_35
action_4 (68) = happyShift action_36
action_4 (70) = happyShift action_2
action_4 (71) = happyShift action_37
action_4 (72) = happyShift action_38
action_4 (4) = happyGoto action_5
action_4 (5) = happyGoto action_6
action_4 (6) = happyGoto action_7
action_4 (10) = happyGoto action_8
action_4 (12) = happyGoto action_9
action_4 (15) = happyGoto action_10
action_4 (19) = happyGoto action_11
action_4 (20) = happyGoto action_12
action_4 (21) = happyGoto action_13
action_4 (22) = happyGoto action_14
action_4 (23) = happyGoto action_15
action_4 (24) = happyGoto action_16
action_4 (25) = happyGoto action_17
action_4 _ = happyReduce_4

action_5 (34) = happyShift action_71
action_5 (38) = happyShift action_72
action_5 (41) = happyShift action_73
action_5 (47) = happyShift action_74
action_5 _ = happyReduce_46

action_6 _ = happyReduce_47

action_7 _ = happyReduce_51

action_8 _ = happyReduce_14

action_9 _ = happyReduce_12

action_10 (70) = happyShift action_2
action_10 (4) = happyGoto action_68
action_10 (13) = happyGoto action_69
action_10 (14) = happyGoto action_70
action_10 _ = happyFail (happyExpListPerState 10)

action_11 _ = happyReduce_55

action_12 _ = happyReduce_57

action_13 (32) = happyShift action_65
action_13 (36) = happyShift action_66
action_13 (42) = happyShift action_67
action_13 (28) = happyGoto action_64
action_13 _ = happyReduce_59

action_14 (37) = happyShift action_62
action_14 (40) = happyShift action_63
action_14 (27) = happyGoto action_61
action_14 _ = happyReduce_61

action_15 (31) = happyShift action_54
action_15 (45) = happyShift action_55
action_15 (46) = happyShift action_56
action_15 (48) = happyShift action_57
action_15 (49) = happyShift action_58
action_15 (50) = happyShift action_59
action_15 (53) = happyShift action_60
action_15 (29) = happyGoto action_53
action_15 _ = happyReduce_63

action_16 (61) = happyShift action_52
action_16 _ = happyReduce_66

action_17 (44) = happyShift action_51
action_17 _ = happyFail (happyExpListPerState 17)

action_18 (34) = happyShift action_19
action_18 (58) = happyShift action_27
action_18 (64) = happyShift action_32
action_18 (70) = happyShift action_2
action_18 (71) = happyShift action_37
action_18 (72) = happyShift action_38
action_18 (4) = happyGoto action_40
action_18 (5) = happyGoto action_6
action_18 (6) = happyGoto action_7
action_18 (19) = happyGoto action_50
action_18 _ = happyFail (happyExpListPerState 18)

action_19 (30) = happyShift action_18
action_19 (34) = happyShift action_19
action_19 (40) = happyShift action_20
action_19 (52) = happyShift action_23
action_19 (58) = happyShift action_27
action_19 (64) = happyShift action_32
action_19 (70) = happyShift action_2
action_19 (71) = happyShift action_37
action_19 (72) = happyShift action_38
action_19 (4) = happyGoto action_40
action_19 (5) = happyGoto action_6
action_19 (6) = happyGoto action_7
action_19 (19) = happyGoto action_11
action_19 (20) = happyGoto action_12
action_19 (21) = happyGoto action_13
action_19 (22) = happyGoto action_14
action_19 (23) = happyGoto action_15
action_19 (24) = happyGoto action_16
action_19 (25) = happyGoto action_49
action_19 _ = happyFail (happyExpListPerState 19)

action_20 (34) = happyShift action_19
action_20 (58) = happyShift action_27
action_20 (64) = happyShift action_32
action_20 (70) = happyShift action_2
action_20 (71) = happyShift action_37
action_20 (72) = happyShift action_38
action_20 (4) = happyGoto action_40
action_20 (5) = happyGoto action_6
action_20 (6) = happyGoto action_7
action_20 (19) = happyGoto action_48
action_20 _ = happyFail (happyExpListPerState 20)

action_21 _ = happyReduce_13

action_22 (45) = happyShift action_47
action_22 _ = happyFail (happyExpListPerState 22)

action_23 (34) = happyShift action_46
action_23 _ = happyFail (happyExpListPerState 23)

action_24 _ = happyReduce_35

action_25 _ = happyReduce_24

action_26 _ = happyReduce_25

action_27 _ = happyReduce_49

action_28 (34) = happyShift action_45
action_28 _ = happyFail (happyExpListPerState 28)

action_29 _ = happyReduce_33

action_30 (30) = happyShift action_18
action_30 (34) = happyShift action_19
action_30 (40) = happyShift action_20
action_30 (44) = happyShift action_44
action_30 (52) = happyShift action_23
action_30 (58) = happyShift action_27
action_30 (64) = happyShift action_32
action_30 (70) = happyShift action_2
action_30 (71) = happyShift action_37
action_30 (72) = happyShift action_38
action_30 (4) = happyGoto action_40
action_30 (5) = happyGoto action_6
action_30 (6) = happyGoto action_7
action_30 (19) = happyGoto action_11
action_30 (20) = happyGoto action_12
action_30 (21) = happyGoto action_13
action_30 (22) = happyGoto action_14
action_30 (23) = happyGoto action_15
action_30 (24) = happyGoto action_16
action_30 (25) = happyGoto action_43
action_30 _ = happyFail (happyExpListPerState 30)

action_31 _ = happyReduce_34

action_32 _ = happyReduce_48

action_33 _ = happyReduce_36

action_34 (34) = happyShift action_42
action_34 _ = happyFail (happyExpListPerState 34)

action_35 (30) = happyShift action_18
action_35 (34) = happyShift action_19
action_35 (40) = happyShift action_20
action_35 (52) = happyShift action_23
action_35 (58) = happyShift action_27
action_35 (64) = happyShift action_32
action_35 (70) = happyShift action_2
action_35 (71) = happyShift action_37
action_35 (72) = happyShift action_38
action_35 (4) = happyGoto action_40
action_35 (5) = happyGoto action_6
action_35 (6) = happyGoto action_7
action_35 (19) = happyGoto action_11
action_35 (20) = happyGoto action_12
action_35 (21) = happyGoto action_13
action_35 (22) = happyGoto action_14
action_35 (23) = happyGoto action_15
action_35 (24) = happyGoto action_16
action_35 (25) = happyGoto action_41
action_35 _ = happyFail (happyExpListPerState 35)

action_36 (11) = happyGoto action_39
action_36 _ = happyReduce_11

action_37 _ = happyReduce_2

action_38 _ = happyReduce_3

action_39 (30) = happyShift action_18
action_39 (34) = happyShift action_19
action_39 (40) = happyShift action_20
action_39 (44) = happyShift action_21
action_39 (51) = happyShift action_22
action_39 (52) = happyShift action_23
action_39 (54) = happyShift action_24
action_39 (55) = happyShift action_25
action_39 (56) = happyShift action_26
action_39 (58) = happyShift action_27
action_39 (59) = happyShift action_28
action_39 (60) = happyShift action_29
action_39 (62) = happyShift action_30
action_39 (63) = happyShift action_31
action_39 (64) = happyShift action_32
action_39 (65) = happyShift action_33
action_39 (66) = happyShift action_34
action_39 (67) = happyShift action_35
action_39 (68) = happyShift action_36
action_39 (69) = happyShift action_98
action_39 (70) = happyShift action_2
action_39 (71) = happyShift action_37
action_39 (72) = happyShift action_38
action_39 (4) = happyGoto action_5
action_39 (5) = happyGoto action_6
action_39 (6) = happyGoto action_7
action_39 (10) = happyGoto action_8
action_39 (12) = happyGoto action_9
action_39 (15) = happyGoto action_10
action_39 (19) = happyGoto action_11
action_39 (20) = happyGoto action_12
action_39 (21) = happyGoto action_13
action_39 (22) = happyGoto action_14
action_39 (23) = happyGoto action_15
action_39 (24) = happyGoto action_16
action_39 (25) = happyGoto action_17
action_39 _ = happyFail (happyExpListPerState 39)

action_40 (34) = happyShift action_71
action_40 _ = happyReduce_46

action_41 (44) = happyShift action_97
action_41 _ = happyFail (happyExpListPerState 41)

action_42 (30) = happyShift action_18
action_42 (34) = happyShift action_19
action_42 (40) = happyShift action_20
action_42 (52) = happyShift action_23
action_42 (58) = happyShift action_27
action_42 (64) = happyShift action_32
action_42 (70) = happyShift action_2
action_42 (71) = happyShift action_37
action_42 (72) = happyShift action_38
action_42 (4) = happyGoto action_40
action_42 (5) = happyGoto action_6
action_42 (6) = happyGoto action_7
action_42 (19) = happyGoto action_11
action_42 (20) = happyGoto action_12
action_42 (21) = happyGoto action_13
action_42 (22) = happyGoto action_14
action_42 (23) = happyGoto action_15
action_42 (24) = happyGoto action_16
action_42 (25) = happyGoto action_96
action_42 _ = happyFail (happyExpListPerState 42)

action_43 (44) = happyShift action_95
action_43 _ = happyFail (happyExpListPerState 43)

action_44 _ = happyReduce_20

action_45 (30) = happyShift action_18
action_45 (34) = happyShift action_19
action_45 (40) = happyShift action_20
action_45 (52) = happyShift action_23
action_45 (58) = happyShift action_27
action_45 (64) = happyShift action_32
action_45 (70) = happyShift action_2
action_45 (71) = happyShift action_37
action_45 (72) = happyShift action_38
action_45 (4) = happyGoto action_40
action_45 (5) = happyGoto action_6
action_45 (6) = happyGoto action_7
action_45 (19) = happyGoto action_11
action_45 (20) = happyGoto action_12
action_45 (21) = happyGoto action_13
action_45 (22) = happyGoto action_14
action_45 (23) = happyGoto action_15
action_45 (24) = happyGoto action_16
action_45 (25) = happyGoto action_94
action_45 _ = happyFail (happyExpListPerState 45)

action_46 (51) = happyShift action_22
action_46 (54) = happyShift action_24
action_46 (60) = happyShift action_29
action_46 (63) = happyShift action_31
action_46 (65) = happyShift action_33
action_46 (8) = happyGoto action_91
action_46 (9) = happyGoto action_92
action_46 (15) = happyGoto action_93
action_46 _ = happyReduce_7

action_47 (51) = happyShift action_22
action_47 (54) = happyShift action_24
action_47 (60) = happyShift action_29
action_47 (63) = happyShift action_31
action_47 (65) = happyShift action_33
action_47 (15) = happyGoto action_90
action_47 _ = happyFail (happyExpListPerState 47)

action_48 _ = happyReduce_53

action_49 (35) = happyShift action_89
action_49 _ = happyFail (happyExpListPerState 49)

action_50 _ = happyReduce_54

action_51 _ = happyReduce_26

action_52 (30) = happyShift action_18
action_52 (34) = happyShift action_19
action_52 (40) = happyShift action_20
action_52 (52) = happyShift action_23
action_52 (58) = happyShift action_27
action_52 (64) = happyShift action_32
action_52 (70) = happyShift action_2
action_52 (71) = happyShift action_37
action_52 (72) = happyShift action_38
action_52 (4) = happyGoto action_40
action_52 (5) = happyGoto action_6
action_52 (6) = happyGoto action_7
action_52 (19) = happyGoto action_11
action_52 (20) = happyGoto action_12
action_52 (21) = happyGoto action_13
action_52 (22) = happyGoto action_14
action_52 (23) = happyGoto action_15
action_52 (24) = happyGoto action_16
action_52 (25) = happyGoto action_88
action_52 _ = happyFail (happyExpListPerState 52)

action_53 (30) = happyShift action_18
action_53 (34) = happyShift action_19
action_53 (40) = happyShift action_20
action_53 (58) = happyShift action_27
action_53 (64) = happyShift action_32
action_53 (70) = happyShift action_2
action_53 (71) = happyShift action_37
action_53 (72) = happyShift action_38
action_53 (4) = happyGoto action_40
action_53 (5) = happyGoto action_6
action_53 (6) = happyGoto action_7
action_53 (19) = happyGoto action_11
action_53 (20) = happyGoto action_12
action_53 (21) = happyGoto action_13
action_53 (22) = happyGoto action_87
action_53 _ = happyFail (happyExpListPerState 53)

action_54 _ = happyReduce_80

action_55 _ = happyReduce_75

action_56 _ = happyReduce_76

action_57 _ = happyReduce_79

action_58 _ = happyReduce_77

action_59 _ = happyReduce_78

action_60 (30) = happyShift action_18
action_60 (34) = happyShift action_19
action_60 (40) = happyShift action_20
action_60 (58) = happyShift action_27
action_60 (64) = happyShift action_32
action_60 (70) = happyShift action_2
action_60 (71) = happyShift action_37
action_60 (72) = happyShift action_38
action_60 (4) = happyGoto action_40
action_60 (5) = happyGoto action_6
action_60 (6) = happyGoto action_7
action_60 (19) = happyGoto action_11
action_60 (20) = happyGoto action_12
action_60 (21) = happyGoto action_13
action_60 (22) = happyGoto action_14
action_60 (23) = happyGoto action_15
action_60 (24) = happyGoto action_86
action_60 _ = happyFail (happyExpListPerState 60)

action_61 (30) = happyShift action_18
action_61 (34) = happyShift action_19
action_61 (40) = happyShift action_20
action_61 (58) = happyShift action_27
action_61 (64) = happyShift action_32
action_61 (70) = happyShift action_2
action_61 (71) = happyShift action_37
action_61 (72) = happyShift action_38
action_61 (4) = happyGoto action_40
action_61 (5) = happyGoto action_6
action_61 (6) = happyGoto action_7
action_61 (19) = happyGoto action_11
action_61 (20) = happyGoto action_12
action_61 (21) = happyGoto action_85
action_61 _ = happyFail (happyExpListPerState 61)

action_62 _ = happyReduce_70

action_63 _ = happyReduce_71

action_64 (30) = happyShift action_18
action_64 (34) = happyShift action_19
action_64 (40) = happyShift action_20
action_64 (58) = happyShift action_27
action_64 (64) = happyShift action_32
action_64 (70) = happyShift action_2
action_64 (71) = happyShift action_37
action_64 (72) = happyShift action_38
action_64 (4) = happyGoto action_40
action_64 (5) = happyGoto action_6
action_64 (6) = happyGoto action_7
action_64 (19) = happyGoto action_11
action_64 (20) = happyGoto action_84
action_64 _ = happyFail (happyExpListPerState 64)

action_65 _ = happyReduce_74

action_66 _ = happyReduce_72

action_67 _ = happyReduce_73

action_68 (34) = happyShift action_82
action_68 (47) = happyShift action_83
action_68 _ = happyReduce_29

action_69 (39) = happyShift action_81
action_69 _ = happyReduce_31

action_70 (44) = happyShift action_80
action_70 _ = happyFail (happyExpListPerState 70)

action_71 (30) = happyShift action_18
action_71 (34) = happyShift action_19
action_71 (40) = happyShift action_20
action_71 (52) = happyShift action_23
action_71 (58) = happyShift action_27
action_71 (64) = happyShift action_32
action_71 (70) = happyShift action_2
action_71 (71) = happyShift action_37
action_71 (72) = happyShift action_38
action_71 (4) = happyGoto action_40
action_71 (5) = happyGoto action_6
action_71 (6) = happyGoto action_7
action_71 (19) = happyGoto action_11
action_71 (20) = happyGoto action_12
action_71 (21) = happyGoto action_13
action_71 (22) = happyGoto action_14
action_71 (23) = happyGoto action_15
action_71 (24) = happyGoto action_16
action_71 (25) = happyGoto action_78
action_71 (26) = happyGoto action_79
action_71 _ = happyReduce_67

action_72 (44) = happyShift action_77
action_72 _ = happyFail (happyExpListPerState 72)

action_73 (44) = happyShift action_76
action_73 _ = happyFail (happyExpListPerState 73)

action_74 (30) = happyShift action_18
action_74 (34) = happyShift action_19
action_74 (40) = happyShift action_20
action_74 (52) = happyShift action_23
action_74 (58) = happyShift action_27
action_74 (64) = happyShift action_32
action_74 (70) = happyShift action_2
action_74 (71) = happyShift action_37
action_74 (72) = happyShift action_38
action_74 (4) = happyGoto action_40
action_74 (5) = happyGoto action_6
action_74 (6) = happyGoto action_7
action_74 (19) = happyGoto action_11
action_74 (20) = happyGoto action_12
action_74 (21) = happyGoto action_13
action_74 (22) = happyGoto action_14
action_74 (23) = happyGoto action_15
action_74 (24) = happyGoto action_16
action_74 (25) = happyGoto action_75
action_74 _ = happyFail (happyExpListPerState 74)

action_75 (44) = happyShift action_112
action_75 _ = happyFail (happyExpListPerState 75)

action_76 _ = happyReduce_18

action_77 _ = happyReduce_17

action_78 (39) = happyShift action_111
action_78 _ = happyReduce_68

action_79 (35) = happyShift action_110
action_79 _ = happyFail (happyExpListPerState 79)

action_80 _ = happyReduce_15

action_81 (70) = happyShift action_2
action_81 (4) = happyGoto action_108
action_81 (13) = happyGoto action_69
action_81 (14) = happyGoto action_109
action_81 _ = happyFail (happyExpListPerState 81)

action_82 (51) = happyShift action_22
action_82 (54) = happyShift action_24
action_82 (60) = happyShift action_29
action_82 (63) = happyShift action_31
action_82 (65) = happyShift action_33
action_82 (8) = happyGoto action_91
action_82 (9) = happyGoto action_107
action_82 (15) = happyGoto action_93
action_82 _ = happyReduce_7

action_83 (30) = happyShift action_18
action_83 (34) = happyShift action_19
action_83 (40) = happyShift action_20
action_83 (52) = happyShift action_23
action_83 (58) = happyShift action_27
action_83 (64) = happyShift action_32
action_83 (70) = happyShift action_2
action_83 (71) = happyShift action_37
action_83 (72) = happyShift action_38
action_83 (4) = happyGoto action_40
action_83 (5) = happyGoto action_6
action_83 (6) = happyGoto action_7
action_83 (19) = happyGoto action_11
action_83 (20) = happyGoto action_12
action_83 (21) = happyGoto action_13
action_83 (22) = happyGoto action_14
action_83 (23) = happyGoto action_15
action_83 (24) = happyGoto action_16
action_83 (25) = happyGoto action_106
action_83 _ = happyFail (happyExpListPerState 83)

action_84 _ = happyReduce_56

action_85 (32) = happyShift action_65
action_85 (36) = happyShift action_66
action_85 (42) = happyShift action_67
action_85 (28) = happyGoto action_64
action_85 _ = happyReduce_58

action_86 _ = happyReduce_62

action_87 (37) = happyShift action_62
action_87 (40) = happyShift action_63
action_87 (27) = happyGoto action_61
action_87 _ = happyReduce_60

action_88 _ = happyReduce_64

action_89 _ = happyReduce_52

action_90 (34) = happyShift action_105
action_90 _ = happyFail (happyExpListPerState 90)

action_91 (39) = happyShift action_104
action_91 _ = happyReduce_8

action_92 (35) = happyShift action_103
action_92 _ = happyFail (happyExpListPerState 92)

action_93 (33) = happyShift action_102
action_93 (70) = happyShift action_2
action_93 (4) = happyGoto action_101
action_93 _ = happyFail (happyExpListPerState 93)

action_94 (35) = happyShift action_100
action_94 _ = happyFail (happyExpListPerState 94)

action_95 _ = happyReduce_19

action_96 (35) = happyShift action_99
action_96 _ = happyFail (happyExpListPerState 96)

action_97 _ = happyReduce_27

action_98 _ = happyReduce_10

action_99 (30) = happyShift action_18
action_99 (34) = happyShift action_19
action_99 (40) = happyShift action_20
action_99 (44) = happyShift action_21
action_99 (51) = happyShift action_22
action_99 (52) = happyShift action_23
action_99 (54) = happyShift action_24
action_99 (55) = happyShift action_25
action_99 (56) = happyShift action_26
action_99 (58) = happyShift action_27
action_99 (59) = happyShift action_28
action_99 (60) = happyShift action_29
action_99 (62) = happyShift action_30
action_99 (63) = happyShift action_31
action_99 (64) = happyShift action_32
action_99 (65) = happyShift action_33
action_99 (66) = happyShift action_34
action_99 (67) = happyShift action_35
action_99 (68) = happyShift action_36
action_99 (70) = happyShift action_2
action_99 (71) = happyShift action_37
action_99 (72) = happyShift action_38
action_99 (4) = happyGoto action_5
action_99 (5) = happyGoto action_6
action_99 (6) = happyGoto action_7
action_99 (10) = happyGoto action_8
action_99 (12) = happyGoto action_122
action_99 (15) = happyGoto action_10
action_99 (19) = happyGoto action_11
action_99 (20) = happyGoto action_12
action_99 (21) = happyGoto action_13
action_99 (22) = happyGoto action_14
action_99 (23) = happyGoto action_15
action_99 (24) = happyGoto action_16
action_99 (25) = happyGoto action_17
action_99 _ = happyFail (happyExpListPerState 99)

action_100 (30) = happyShift action_18
action_100 (34) = happyShift action_19
action_100 (40) = happyShift action_20
action_100 (44) = happyShift action_21
action_100 (51) = happyShift action_22
action_100 (52) = happyShift action_23
action_100 (54) = happyShift action_24
action_100 (55) = happyShift action_25
action_100 (56) = happyShift action_26
action_100 (58) = happyShift action_27
action_100 (59) = happyShift action_28
action_100 (60) = happyShift action_29
action_100 (62) = happyShift action_30
action_100 (63) = happyShift action_31
action_100 (64) = happyShift action_32
action_100 (65) = happyShift action_33
action_100 (66) = happyShift action_34
action_100 (67) = happyShift action_35
action_100 (68) = happyShift action_36
action_100 (70) = happyShift action_2
action_100 (71) = happyShift action_37
action_100 (72) = happyShift action_38
action_100 (4) = happyGoto action_5
action_100 (5) = happyGoto action_6
action_100 (6) = happyGoto action_7
action_100 (10) = happyGoto action_8
action_100 (12) = happyGoto action_121
action_100 (15) = happyGoto action_10
action_100 (19) = happyGoto action_11
action_100 (20) = happyGoto action_12
action_100 (21) = happyGoto action_13
action_100 (22) = happyGoto action_14
action_100 (23) = happyGoto action_15
action_100 (24) = happyGoto action_16
action_100 (25) = happyGoto action_17
action_100 _ = happyFail (happyExpListPerState 100)

action_101 _ = happyReduce_5

action_102 (70) = happyShift action_2
action_102 (4) = happyGoto action_120
action_102 _ = happyFail (happyExpListPerState 102)

action_103 (43) = happyShift action_119
action_103 _ = happyFail (happyExpListPerState 103)

action_104 (51) = happyShift action_22
action_104 (54) = happyShift action_24
action_104 (60) = happyShift action_29
action_104 (63) = happyShift action_31
action_104 (65) = happyShift action_33
action_104 (8) = happyGoto action_91
action_104 (9) = happyGoto action_118
action_104 (15) = happyGoto action_93
action_104 _ = happyReduce_7

action_105 (51) = happyShift action_22
action_105 (54) = happyShift action_24
action_105 (60) = happyShift action_29
action_105 (63) = happyShift action_31
action_105 (65) = happyShift action_33
action_105 (15) = happyGoto action_115
action_105 (16) = happyGoto action_116
action_105 (17) = happyGoto action_117
action_105 _ = happyReduce_40

action_106 _ = happyReduce_30

action_107 (35) = happyShift action_114
action_107 _ = happyFail (happyExpListPerState 107)

action_108 (47) = happyShift action_83
action_108 _ = happyReduce_29

action_109 _ = happyReduce_32

action_110 _ = happyReduce_50

action_111 (30) = happyShift action_18
action_111 (34) = happyShift action_19
action_111 (40) = happyShift action_20
action_111 (52) = happyShift action_23
action_111 (58) = happyShift action_27
action_111 (64) = happyShift action_32
action_111 (70) = happyShift action_2
action_111 (71) = happyShift action_37
action_111 (72) = happyShift action_38
action_111 (4) = happyGoto action_40
action_111 (5) = happyGoto action_6
action_111 (6) = happyGoto action_7
action_111 (19) = happyGoto action_11
action_111 (20) = happyGoto action_12
action_111 (21) = happyGoto action_13
action_111 (22) = happyGoto action_14
action_111 (23) = happyGoto action_15
action_111 (24) = happyGoto action_16
action_111 (25) = happyGoto action_78
action_111 (26) = happyGoto action_113
action_111 _ = happyReduce_67

action_112 _ = happyReduce_16

action_113 _ = happyReduce_69

action_114 (68) = happyShift action_36
action_114 (10) = happyGoto action_128
action_114 _ = happyFail (happyExpListPerState 114)

action_115 (33) = happyShift action_127
action_115 _ = happyReduce_38

action_116 (39) = happyShift action_126
action_116 _ = happyReduce_41

action_117 (35) = happyShift action_125
action_117 _ = happyFail (happyExpListPerState 117)

action_118 _ = happyReduce_9

action_119 (51) = happyShift action_22
action_119 (54) = happyShift action_24
action_119 (60) = happyShift action_29
action_119 (63) = happyShift action_31
action_119 (65) = happyShift action_33
action_119 (15) = happyGoto action_124
action_119 _ = happyFail (happyExpListPerState 119)

action_120 _ = happyReduce_6

action_121 (57) = happyShift action_123
action_121 _ = happyReduce_21

action_122 _ = happyReduce_23

action_123 (30) = happyShift action_18
action_123 (34) = happyShift action_19
action_123 (40) = happyShift action_20
action_123 (44) = happyShift action_21
action_123 (51) = happyShift action_22
action_123 (52) = happyShift action_23
action_123 (54) = happyShift action_24
action_123 (55) = happyShift action_25
action_123 (56) = happyShift action_26
action_123 (58) = happyShift action_27
action_123 (59) = happyShift action_28
action_123 (60) = happyShift action_29
action_123 (62) = happyShift action_30
action_123 (63) = happyShift action_31
action_123 (64) = happyShift action_32
action_123 (65) = happyShift action_33
action_123 (66) = happyShift action_34
action_123 (67) = happyShift action_35
action_123 (68) = happyShift action_36
action_123 (70) = happyShift action_2
action_123 (71) = happyShift action_37
action_123 (72) = happyShift action_38
action_123 (4) = happyGoto action_5
action_123 (5) = happyGoto action_6
action_123 (6) = happyGoto action_7
action_123 (10) = happyGoto action_8
action_123 (12) = happyGoto action_132
action_123 (15) = happyGoto action_10
action_123 (19) = happyGoto action_11
action_123 (20) = happyGoto action_12
action_123 (21) = happyGoto action_13
action_123 (22) = happyGoto action_14
action_123 (23) = happyGoto action_15
action_123 (24) = happyGoto action_16
action_123 (25) = happyGoto action_17
action_123 _ = happyFail (happyExpListPerState 123)

action_124 (68) = happyShift action_36
action_124 (10) = happyGoto action_131
action_124 _ = happyFail (happyExpListPerState 124)

action_125 (49) = happyShift action_130
action_125 _ = happyFail (happyExpListPerState 125)

action_126 (51) = happyShift action_22
action_126 (54) = happyShift action_24
action_126 (60) = happyShift action_29
action_126 (63) = happyShift action_31
action_126 (65) = happyShift action_33
action_126 (15) = happyGoto action_115
action_126 (16) = happyGoto action_116
action_126 (17) = happyGoto action_129
action_126 _ = happyReduce_40

action_127 _ = happyReduce_39

action_128 _ = happyReduce_28

action_129 _ = happyReduce_42

action_130 _ = happyReduce_37

action_131 _ = happyReduce_65

action_132 _ = happyReduce_22

happyReduce_1 = happySpecReduce_1  4 happyReduction_1
happyReduction_1 (HappyTerminal (PT _ (TV happy_var_1)))
	 =  HappyAbsSyn4
		 (Ident happy_var_1
	)
happyReduction_1 _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_1  5 happyReduction_2
happyReduction_2 (HappyTerminal (PT _ (TI happy_var_1)))
	 =  HappyAbsSyn5
		 (read happy_var_1
	)
happyReduction_2 _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_1  6 happyReduction_3
happyReduction_3 (HappyTerminal (PT _ (TL happy_var_1)))
	 =  HappyAbsSyn6
		 (happy_var_1
	)
happyReduction_3 _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_1  7 happyReduction_4
happyReduction_4 (HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn7
		 (Samoyeet.Abs.Program (reverse happy_var_1)
	)
happyReduction_4 _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_2  8 happyReduction_5
happyReduction_5 (HappyAbsSyn4  happy_var_2)
	(HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn8
		 (Samoyeet.Abs.Arg happy_var_1 happy_var_2
	)
happyReduction_5 _ _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_3  8 happyReduction_6
happyReduction_6 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn8
		 (Samoyeet.Abs.RefArg happy_var_1 happy_var_3
	)
happyReduction_6 _ _ _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_0  9 happyReduction_7
happyReduction_7  =  HappyAbsSyn9
		 ([]
	)

happyReduce_8 = happySpecReduce_1  9 happyReduction_8
happyReduction_8 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn9
		 ((:[]) happy_var_1
	)
happyReduction_8 _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_3  9 happyReduction_9
happyReduction_9 (HappyAbsSyn9  happy_var_3)
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn9
		 ((:) happy_var_1 happy_var_3
	)
happyReduction_9 _ _ _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_3  10 happyReduction_10
happyReduction_10 _
	(HappyAbsSyn11  happy_var_2)
	_
	 =  HappyAbsSyn10
		 (Samoyeet.Abs.Block (reverse happy_var_2)
	)
happyReduction_10 _ _ _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_0  11 happyReduction_11
happyReduction_11  =  HappyAbsSyn11
		 ([]
	)

happyReduce_12 = happySpecReduce_2  11 happyReduction_12
happyReduction_12 (HappyAbsSyn12  happy_var_2)
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (flip (:) happy_var_1 happy_var_2
	)
happyReduction_12 _ _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_1  12 happyReduction_13
happyReduction_13 _
	 =  HappyAbsSyn12
		 (Samoyeet.Abs.Empty
	)

happyReduce_14 = happySpecReduce_1  12 happyReduction_14
happyReduction_14 (HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn12
		 (Samoyeet.Abs.BStmt happy_var_1
	)
happyReduction_14 _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_3  12 happyReduction_15
happyReduction_15 _
	(HappyAbsSyn14  happy_var_2)
	(HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn12
		 (Samoyeet.Abs.Decl happy_var_1 happy_var_2
	)
happyReduction_15 _ _ _  = notHappyAtAll 

happyReduce_16 = happyReduce 4 12 happyReduction_16
happyReduction_16 (_ `HappyStk`
	(HappyAbsSyn19  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn12
		 (Samoyeet.Abs.Ass happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_17 = happySpecReduce_3  12 happyReduction_17
happyReduction_17 _
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn12
		 (Samoyeet.Abs.Incr happy_var_1
	)
happyReduction_17 _ _ _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_3  12 happyReduction_18
happyReduction_18 _
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn12
		 (Samoyeet.Abs.Decr happy_var_1
	)
happyReduction_18 _ _ _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_3  12 happyReduction_19
happyReduction_19 _
	(HappyAbsSyn19  happy_var_2)
	_
	 =  HappyAbsSyn12
		 (Samoyeet.Abs.Ret happy_var_2
	)
happyReduction_19 _ _ _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_2  12 happyReduction_20
happyReduction_20 _
	_
	 =  HappyAbsSyn12
		 (Samoyeet.Abs.VRet
	)

happyReduce_21 = happyReduce 5 12 happyReduction_21
happyReduction_21 ((HappyAbsSyn12  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn19  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn12
		 (Samoyeet.Abs.Cond happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_22 = happyReduce 7 12 happyReduction_22
happyReduction_22 ((HappyAbsSyn12  happy_var_7) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn12  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn19  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn12
		 (Samoyeet.Abs.CondElse happy_var_3 happy_var_5 happy_var_7
	) `HappyStk` happyRest

happyReduce_23 = happyReduce 5 12 happyReduction_23
happyReduction_23 ((HappyAbsSyn12  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn19  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn12
		 (Samoyeet.Abs.While happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_24 = happySpecReduce_1  12 happyReduction_24
happyReduction_24 _
	 =  HappyAbsSyn12
		 (Samoyeet.Abs.SBreak
	)

happyReduce_25 = happySpecReduce_1  12 happyReduction_25
happyReduction_25 _
	 =  HappyAbsSyn12
		 (Samoyeet.Abs.SContinue
	)

happyReduce_26 = happySpecReduce_2  12 happyReduction_26
happyReduction_26 _
	(HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn12
		 (Samoyeet.Abs.SExp happy_var_1
	)
happyReduction_26 _ _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_3  12 happyReduction_27
happyReduction_27 _
	(HappyAbsSyn19  happy_var_2)
	_
	 =  HappyAbsSyn12
		 (Samoyeet.Abs.Print happy_var_2
	)
happyReduction_27 _ _ _  = notHappyAtAll 

happyReduce_28 = happyReduce 6 12 happyReduction_28
happyReduction_28 ((HappyAbsSyn10  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn9  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_2) `HappyStk`
	(HappyAbsSyn15  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn12
		 (Samoyeet.Abs.SFnDef happy_var_1 happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_29 = happySpecReduce_1  13 happyReduction_29
happyReduction_29 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn13
		 (Samoyeet.Abs.NoInit happy_var_1
	)
happyReduction_29 _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_3  13 happyReduction_30
happyReduction_30 (HappyAbsSyn19  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn13
		 (Samoyeet.Abs.Init happy_var_1 happy_var_3
	)
happyReduction_30 _ _ _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_1  14 happyReduction_31
happyReduction_31 (HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn14
		 ((:[]) happy_var_1
	)
happyReduction_31 _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_3  14 happyReduction_32
happyReduction_32 (HappyAbsSyn14  happy_var_3)
	_
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn14
		 ((:) happy_var_1 happy_var_3
	)
happyReduction_32 _ _ _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_1  15 happyReduction_33
happyReduction_33 _
	 =  HappyAbsSyn15
		 (Samoyeet.Abs.Int
	)

happyReduce_34 = happySpecReduce_1  15 happyReduction_34
happyReduction_34 _
	 =  HappyAbsSyn15
		 (Samoyeet.Abs.Str
	)

happyReduce_35 = happySpecReduce_1  15 happyReduction_35
happyReduction_35 _
	 =  HappyAbsSyn15
		 (Samoyeet.Abs.Bool
	)

happyReduce_36 = happySpecReduce_1  15 happyReduction_36
happyReduction_36 _
	 =  HappyAbsSyn15
		 (Samoyeet.Abs.Void
	)

happyReduce_37 = happyReduce 7 15 happyReduction_37
happyReduction_37 (_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn17  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn15  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn15
		 (Samoyeet.Abs.Fun happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_38 = happySpecReduce_1  16 happyReduction_38
happyReduction_38 (HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn16
		 (Samoyeet.Abs.NoRef happy_var_1
	)
happyReduction_38 _  = notHappyAtAll 

happyReduce_39 = happySpecReduce_2  16 happyReduction_39
happyReduction_39 _
	(HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn16
		 (Samoyeet.Abs.JustRef happy_var_1
	)
happyReduction_39 _ _  = notHappyAtAll 

happyReduce_40 = happySpecReduce_0  17 happyReduction_40
happyReduction_40  =  HappyAbsSyn17
		 ([]
	)

happyReduce_41 = happySpecReduce_1  17 happyReduction_41
happyReduction_41 (HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn17
		 ((:[]) happy_var_1
	)
happyReduction_41 _  = notHappyAtAll 

happyReduce_42 = happySpecReduce_3  17 happyReduction_42
happyReduction_42 (HappyAbsSyn17  happy_var_3)
	_
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn17
		 ((:) happy_var_1 happy_var_3
	)
happyReduction_42 _ _ _  = notHappyAtAll 

happyReduce_43 = happySpecReduce_0  18 happyReduction_43
happyReduction_43  =  HappyAbsSyn18
		 ([]
	)

happyReduce_44 = happySpecReduce_1  18 happyReduction_44
happyReduction_44 (HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn18
		 ((:[]) happy_var_1
	)
happyReduction_44 _  = notHappyAtAll 

happyReduce_45 = happySpecReduce_3  18 happyReduction_45
happyReduction_45 (HappyAbsSyn18  happy_var_3)
	_
	(HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn18
		 ((:) happy_var_1 happy_var_3
	)
happyReduction_45 _ _ _  = notHappyAtAll 

happyReduce_46 = happySpecReduce_1  19 happyReduction_46
happyReduction_46 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn19
		 (Samoyeet.Abs.EVar happy_var_1
	)
happyReduction_46 _  = notHappyAtAll 

happyReduce_47 = happySpecReduce_1  19 happyReduction_47
happyReduction_47 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn19
		 (Samoyeet.Abs.ELitInt happy_var_1
	)
happyReduction_47 _  = notHappyAtAll 

happyReduce_48 = happySpecReduce_1  19 happyReduction_48
happyReduction_48 _
	 =  HappyAbsSyn19
		 (Samoyeet.Abs.ELitTrue
	)

happyReduce_49 = happySpecReduce_1  19 happyReduction_49
happyReduction_49 _
	 =  HappyAbsSyn19
		 (Samoyeet.Abs.ELitFalse
	)

happyReduce_50 = happyReduce 4 19 happyReduction_50
happyReduction_50 (_ `HappyStk`
	(HappyAbsSyn26  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn19
		 (Samoyeet.Abs.EApp happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_51 = happySpecReduce_1  19 happyReduction_51
happyReduction_51 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn19
		 (Samoyeet.Abs.EString happy_var_1
	)
happyReduction_51 _  = notHappyAtAll 

happyReduce_52 = happySpecReduce_3  19 happyReduction_52
happyReduction_52 _
	(HappyAbsSyn19  happy_var_2)
	_
	 =  HappyAbsSyn19
		 (happy_var_2
	)
happyReduction_52 _ _ _  = notHappyAtAll 

happyReduce_53 = happySpecReduce_2  20 happyReduction_53
happyReduction_53 (HappyAbsSyn19  happy_var_2)
	_
	 =  HappyAbsSyn19
		 (Samoyeet.Abs.Neg happy_var_2
	)
happyReduction_53 _ _  = notHappyAtAll 

happyReduce_54 = happySpecReduce_2  20 happyReduction_54
happyReduction_54 (HappyAbsSyn19  happy_var_2)
	_
	 =  HappyAbsSyn19
		 (Samoyeet.Abs.Not happy_var_2
	)
happyReduction_54 _ _  = notHappyAtAll 

happyReduce_55 = happySpecReduce_1  20 happyReduction_55
happyReduction_55 (HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn19
		 (happy_var_1
	)
happyReduction_55 _  = notHappyAtAll 

happyReduce_56 = happySpecReduce_3  21 happyReduction_56
happyReduction_56 (HappyAbsSyn19  happy_var_3)
	(HappyAbsSyn28  happy_var_2)
	(HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn19
		 (Samoyeet.Abs.EMul happy_var_1 happy_var_2 happy_var_3
	)
happyReduction_56 _ _ _  = notHappyAtAll 

happyReduce_57 = happySpecReduce_1  21 happyReduction_57
happyReduction_57 (HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn19
		 (happy_var_1
	)
happyReduction_57 _  = notHappyAtAll 

happyReduce_58 = happySpecReduce_3  22 happyReduction_58
happyReduction_58 (HappyAbsSyn19  happy_var_3)
	(HappyAbsSyn27  happy_var_2)
	(HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn19
		 (Samoyeet.Abs.EAdd happy_var_1 happy_var_2 happy_var_3
	)
happyReduction_58 _ _ _  = notHappyAtAll 

happyReduce_59 = happySpecReduce_1  22 happyReduction_59
happyReduction_59 (HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn19
		 (happy_var_1
	)
happyReduction_59 _  = notHappyAtAll 

happyReduce_60 = happySpecReduce_3  23 happyReduction_60
happyReduction_60 (HappyAbsSyn19  happy_var_3)
	(HappyAbsSyn29  happy_var_2)
	(HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn19
		 (Samoyeet.Abs.ERel happy_var_1 happy_var_2 happy_var_3
	)
happyReduction_60 _ _ _  = notHappyAtAll 

happyReduce_61 = happySpecReduce_1  23 happyReduction_61
happyReduction_61 (HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn19
		 (happy_var_1
	)
happyReduction_61 _  = notHappyAtAll 

happyReduce_62 = happySpecReduce_3  24 happyReduction_62
happyReduction_62 (HappyAbsSyn19  happy_var_3)
	_
	(HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn19
		 (Samoyeet.Abs.EAnd happy_var_1 happy_var_3
	)
happyReduction_62 _ _ _  = notHappyAtAll 

happyReduce_63 = happySpecReduce_1  24 happyReduction_63
happyReduction_63 (HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn19
		 (happy_var_1
	)
happyReduction_63 _  = notHappyAtAll 

happyReduce_64 = happySpecReduce_3  25 happyReduction_64
happyReduction_64 (HappyAbsSyn19  happy_var_3)
	_
	(HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn19
		 (Samoyeet.Abs.EOr happy_var_1 happy_var_3
	)
happyReduction_64 _ _ _  = notHappyAtAll 

happyReduce_65 = happyReduce 7 25 happyReduction_65
happyReduction_65 ((HappyAbsSyn10  happy_var_7) `HappyStk`
	(HappyAbsSyn15  happy_var_6) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn9  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn19
		 (Samoyeet.Abs.ELambda happy_var_3 happy_var_6 happy_var_7
	) `HappyStk` happyRest

happyReduce_66 = happySpecReduce_1  25 happyReduction_66
happyReduction_66 (HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn19
		 (happy_var_1
	)
happyReduction_66 _  = notHappyAtAll 

happyReduce_67 = happySpecReduce_0  26 happyReduction_67
happyReduction_67  =  HappyAbsSyn26
		 ([]
	)

happyReduce_68 = happySpecReduce_1  26 happyReduction_68
happyReduction_68 (HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn26
		 ((:[]) happy_var_1
	)
happyReduction_68 _  = notHappyAtAll 

happyReduce_69 = happySpecReduce_3  26 happyReduction_69
happyReduction_69 (HappyAbsSyn26  happy_var_3)
	_
	(HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn26
		 ((:) happy_var_1 happy_var_3
	)
happyReduction_69 _ _ _  = notHappyAtAll 

happyReduce_70 = happySpecReduce_1  27 happyReduction_70
happyReduction_70 _
	 =  HappyAbsSyn27
		 (Samoyeet.Abs.Plus
	)

happyReduce_71 = happySpecReduce_1  27 happyReduction_71
happyReduction_71 _
	 =  HappyAbsSyn27
		 (Samoyeet.Abs.Minus
	)

happyReduce_72 = happySpecReduce_1  28 happyReduction_72
happyReduction_72 _
	 =  HappyAbsSyn28
		 (Samoyeet.Abs.Times
	)

happyReduce_73 = happySpecReduce_1  28 happyReduction_73
happyReduction_73 _
	 =  HappyAbsSyn28
		 (Samoyeet.Abs.Div
	)

happyReduce_74 = happySpecReduce_1  28 happyReduction_74
happyReduction_74 _
	 =  HappyAbsSyn28
		 (Samoyeet.Abs.Mod
	)

happyReduce_75 = happySpecReduce_1  29 happyReduction_75
happyReduction_75 _
	 =  HappyAbsSyn29
		 (Samoyeet.Abs.LTH
	)

happyReduce_76 = happySpecReduce_1  29 happyReduction_76
happyReduction_76 _
	 =  HappyAbsSyn29
		 (Samoyeet.Abs.LE
	)

happyReduce_77 = happySpecReduce_1  29 happyReduction_77
happyReduction_77 _
	 =  HappyAbsSyn29
		 (Samoyeet.Abs.GTH
	)

happyReduce_78 = happySpecReduce_1  29 happyReduction_78
happyReduction_78 _
	 =  HappyAbsSyn29
		 (Samoyeet.Abs.GE
	)

happyReduce_79 = happySpecReduce_1  29 happyReduction_79
happyReduction_79 _
	 =  HappyAbsSyn29
		 (Samoyeet.Abs.EQU
	)

happyReduce_80 = happySpecReduce_1  29 happyReduction_80
happyReduction_80 _
	 =  HappyAbsSyn29
		 (Samoyeet.Abs.NE
	)

happyNewToken action sts stk [] =
	action 73 73 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	PT _ (TS _ 1) -> cont 30;
	PT _ (TS _ 2) -> cont 31;
	PT _ (TS _ 3) -> cont 32;
	PT _ (TS _ 4) -> cont 33;
	PT _ (TS _ 5) -> cont 34;
	PT _ (TS _ 6) -> cont 35;
	PT _ (TS _ 7) -> cont 36;
	PT _ (TS _ 8) -> cont 37;
	PT _ (TS _ 9) -> cont 38;
	PT _ (TS _ 10) -> cont 39;
	PT _ (TS _ 11) -> cont 40;
	PT _ (TS _ 12) -> cont 41;
	PT _ (TS _ 13) -> cont 42;
	PT _ (TS _ 14) -> cont 43;
	PT _ (TS _ 15) -> cont 44;
	PT _ (TS _ 16) -> cont 45;
	PT _ (TS _ 17) -> cont 46;
	PT _ (TS _ 18) -> cont 47;
	PT _ (TS _ 19) -> cont 48;
	PT _ (TS _ 20) -> cont 49;
	PT _ (TS _ 21) -> cont 50;
	PT _ (TS _ 22) -> cont 51;
	PT _ (TS _ 23) -> cont 52;
	PT _ (TS _ 24) -> cont 53;
	PT _ (TS _ 25) -> cont 54;
	PT _ (TS _ 26) -> cont 55;
	PT _ (TS _ 27) -> cont 56;
	PT _ (TS _ 28) -> cont 57;
	PT _ (TS _ 29) -> cont 58;
	PT _ (TS _ 30) -> cont 59;
	PT _ (TS _ 31) -> cont 60;
	PT _ (TS _ 32) -> cont 61;
	PT _ (TS _ 33) -> cont 62;
	PT _ (TS _ 34) -> cont 63;
	PT _ (TS _ 35) -> cont 64;
	PT _ (TS _ 36) -> cont 65;
	PT _ (TS _ 37) -> cont 66;
	PT _ (TS _ 38) -> cont 67;
	PT _ (TS _ 39) -> cont 68;
	PT _ (TS _ 40) -> cont 69;
	PT _ (TV happy_dollar_dollar) -> cont 70;
	PT _ (TI happy_dollar_dollar) -> cont 71;
	PT _ (TL happy_dollar_dollar) -> cont 72;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 73 tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

happyThen :: () => Err a -> (a -> Err b) -> Err b
happyThen = (thenM)
happyReturn :: () => a -> Err a
happyReturn = (returnM)
happyThen1 m k tks = (thenM) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> Err a
happyReturn1 = \a tks -> (returnM) a
happyError' :: () => ([(Token)], [String]) -> Err a
happyError' = (\(tokens, _) -> happyError tokens)
pProgram tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn7 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


returnM :: a -> Err a
returnM = return

thenM :: Err a -> (a -> Err b) -> Err b
thenM = (>>=)

happyError :: [Token] -> Err a
happyError ts =
  Bad $ "syntax error at " ++ tokenPos ts ++ 
  case ts of
    [] -> []
    [Err _] -> " due to lexer error"
    t:_ -> " before `" ++ id(prToken t) ++ "'"

myLexer = tokens
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
