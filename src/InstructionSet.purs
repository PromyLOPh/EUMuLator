-- | EUMEL0 instruction set data types
module InstructionSet where

import Prelude
import Data.Show
import Data.Generic.Rep.Show
import Data.Generic.Rep

-- in `Machine.wordsize` bytes
maxInstructionLen = 1+7

-- Operands:
-- Immediate, i.e. data encoded in instruction word
type Immediate = Int
-- Module number
type Module = Int
type CodeAddress = Int
type Procedure = Int

-- A data memory address; XXX: uses word-addressing?
data DataAddress =
	  GlobalDataAddress Int
	| LocalDataAddress Int
	| LocalDataAddressRef Int

-- No operand
data Op0 =
	  ARITS
	| ARITU
	| CLRERR
	| DSTOP
	| ERTAB
	| ESTOP
	| GARB
	| GETTAB
	| GORET
	| ISERR
    | KE
	| PUTTAB
	| RTN
	| RTNF
	| RTNT
	| SHUTUP
    | STOP
	| SYSG
	| XREW

-- One immediate Operand
data OpI =
	  LN
	| LN1
    | PENTER

-- One CodeAddress operand
data OpC =
	  B
	| B1
	| GOSUB

-- One Module operand
data OpM =
      CALL
	| EXEC
	| PPROC

-- One DataAddress operand
data OpD =
	  ACT
	| CHAEX
	| CLEAR
	| COUT
	| DEACT
	| DEC1
	| DEFCOL
	| DSFORG
	| EXTASK
	| HPSIZE
	| INC1 
	| INCETY
	| INCHAR
	| ISDIG
	| ISLCAS
	| ISLD
	| ISSHA
	| ISUCAS
	| NILDS
	| OUT
	| PAUSE
	| PCALL
	| PNACT
    | PP
    | PPV
	| SESSION
	| SETERR
	| SETNOW
	| SYSOP
	| TEND
	| TEST
	| THALT
	| XERR -- XXX not sure about the first letter, IpagesDT in src/devel/misc/unknown/src/0DISASS.ELA
	| XSKIP

-- Two DataAddress operands
data OpDD =
	  AMUL256
    | BCRD
	| CAT
	| CATINP
	| CDBINT
	| CDBTXT
	| CLOCK
    | CRD
	| CTT
	| DEC
	| DECOD
	| DREM256
	| DSACC
	| DSCOPY
	| DSHEAP
	| DSRTYP
	| DSWTYP
	| ENCOD
	| EQU
	| FEQU
	| FLOOR
	| FLSEQ
	| FMOV
	| FNEG
	| GCPOS
	| GEXP
	| INC
	| ID
	| LSEQ
	| MOV
	| NEG
	| OUTF
	| REF
	| ROTATE
	| RPCB
	| SEXP
	| STORAGE
	| TBEG
	| TCPU
	| TEQU
	| TLEN
	| TLSEQ
	| TMOV
	| TSTAT
	| TWCPU
	| ULSEQ
	| WPCB

data OpID =
      EQUIM
	| MOVII
    | MOVI

data OpDI =
	  BRCOMP

data OpDDD =
	  ADD
	| AND
	| CWR
	| DIV
	| DSPAGS
    | ECWR
	| FADD
	| FDIV
	| FMUL
	| FNONBL
	| FSLD
	| FSUB
	| GADDR
	| GCADDR
	| GETC
	| GW
	| IMULT
	| INFOPW
	| ITRPL
	| ITSUB
	| MOD
	| MUL
	| NXTDSP
	| OR
	| OUTFT
	| POS
	| PW
	| REPLAC
	| RTRPL
	| RTSUB
	| SUB
	| SUBT1
	| SUBTF
	| TRPCB
	| TWPCB
	| WAIT
	| XOR
	| XREAD
	| XWRITE

data OpDID =
	  SEL

data OpIDD =
	  ALIAS
    | MOVX
	| MOVXX

data OpIIDD =
      GETW
    | PUTW

data OpDDDD =
	  CONTRL
	| POSF
	| PPCALL
	| SEND
	| SUBTFT
	| SWCALL

data OpDDDC = TPBEGIN

data OpIIDDD =
	  SUBS

data OpDDDDD =
	  BLKIN
	| BLKOUT
	| SENDFT
	| POSFT
	| POSIF

data OpDDDDDDD = STRANL

-- | Instruction encodings
-- | A single Instruction Opcode (Array Args) would be sufficient, but not
-- | type-safe. Thus they are grouped by encoding (i.e. argument count and type).
data Instruction =
	  Instruction0 Op0
	| InstructionI OpI Immediate
	| InstructionC OpC CodeAddress
	| InstructionM OpM Module
	| InstructionD OpD DataAddress
	| InstructionDD OpDD DataAddress DataAddress
	| InstructionID OpID Immediate DataAddress
	| InstructionDI OpDI DataAddress Immediate 
	| InstructionDDD OpDDD DataAddress DataAddress DataAddress
	| InstructionDID OpDID DataAddress Immediate DataAddress
	| InstructionIDD OpIDD Immediate DataAddress DataAddress
	| InstructionIIDD OpIIDD Immediate Immediate DataAddress DataAddress
	| InstructionDDDD OpDDDD DataAddress DataAddress DataAddress DataAddress
	| InstructionDDDC OpDDDC DataAddress DataAddress DataAddress CodeAddress
	| InstructionIIDDD OpIIDDD Immediate Immediate DataAddress DataAddress DataAddress
	| InstructionDDDDD OpDDDDD DataAddress DataAddress DataAddress DataAddress DataAddress
	| InstructionDDDDDDD OpDDDDDDD DataAddress DataAddress DataAddress DataAddress DataAddress DataAddress DataAddress

-- Until we figure out how to do this without generics
derive instance genericOp0 :: Generic Op0 _
instance showOp0 :: Show Op0 where
    show = genericShow

derive instance genericOpI :: Generic OpI _
instance showOpI :: Show OpI where
    show = genericShow

derive instance genericOpC :: Generic OpC _
instance showOpC :: Show OpC where
    show = genericShow

derive instance genericOpM :: Generic OpM _
instance showOpM :: Show OpM where
    show = genericShow

derive instance genericOpD :: Generic OpD _
instance showOpD :: Show OpD where
    show = genericShow

derive instance genericOpDD :: Generic OpDD _
instance showOpDD :: Show OpDD where
    show = genericShow

derive instance genericOpID :: Generic OpID _
instance showOpID :: Show OpID where
    show = genericShow

derive instance genericOpDI :: Generic OpDI _
instance showOpDI :: Show OpDI where
    show = genericShow

derive instance genericOpDDD :: Generic OpDDD _
instance showOpDDD :: Show OpDDD where
    show = genericShow

derive instance genericOpDID :: Generic OpDID _
instance showOpDID :: Show OpDID where
    show = genericShow

derive instance genericOpIDD :: Generic OpIDD _
instance showOpIDD :: Show OpIDD where
    show = genericShow

derive instance genericOpIIDD :: Generic OpIIDD _
instance showOpIIDD :: Show OpIIDD where
    show = genericShow

derive instance genericOpDDDD :: Generic OpDDDD _
instance showOpDDDD :: Show OpDDDD where
    show = genericShow

derive instance genericOpDDDC :: Generic OpDDDC _
instance showOpDDDC :: Show OpDDDC where
    show = genericShow

derive instance genericOpDDDDD :: Generic OpDDDDD _
instance showOpDDDDD :: Show OpDDDDD where
    show = genericShow

derive instance genericOpIIDDD :: Generic OpIIDDD _
instance showOpIIDDD :: Show OpIIDDD where
    show = genericShow

derive instance genericOpDDDDDDD :: Generic OpDDDDDDD _
instance showOpDDDDDDD :: Show OpDDDDDDD where
    show = genericShow

