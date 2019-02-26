-- Decode BIT-A encoded EUMEL0 instuction set.
--
-- library/entwurf-systemdokumentation-1982.djvu documents the basics of this
-- simple, word-based, variable-length encoding.
--
-- Some instructions are not documented in the system documentation or differ
-- from the documentation there. Apparently the instruction set was changed
-- starting with EUMEL 1.7.5.4 (see comment in
-- src/devel/debugger/src/DEBUGGER.ELA). For the new/changed instructions see
-- src/devel/misc/unknown/src/0DISASS.ELA and
-- src/devel/debugger/doc/DEBUGGER.PRT

module Decode (decode, DecodeError(..), Opcode, Word8) where

import Prelude hiding (apply)
import Data.Tuple
import Data.Int.Bits
import Data.List
import Data.Either
import Data.Function (apply)

import InstructionSet

-- There is a Data.Word module, but it is outdated, so we cannot use that
type Word4 = Int
type Word8 = Int
type Word16 = Int
type Opcode = Word8

-- Number of words consumed by decoding
type Consumed = Int
type DecodeResult = Either DecodeError (Tuple Instruction Consumed)

data DecodeError =
	-- Expected number of words
	  InputTooShort Int
	| UnknownOpcode Opcode

-- | Decode arbitrary EUMEL0 instruction from a list of Word16, returns instruction and number of word16 consumed
-- XXX: would be nice to use generics here and automate argument parsing (since every argument can only be 16 bit)
decode :: List Word16 -> DecodeResult
decode (first:xs) = decode' high low xs
	where
		high = and (shr first 8) 0xff
		low = and first 0xff
decode xs = makeInputTooShort 0

-- | Decode instruction with first Word16 split into two Word8. Helper for real decode.
decode' :: Word8 -> Word8 -> List Word16 -> DecodeResult
-- special instructions
decode' 0x7c v xs = makeInstID EQUIM (v:xs)
decode' 0xfc v xs = makeInstID MOVI (v:xs)
decode' 0x7d v xs = makeInstIDD MOVX (v:xs)
-- two args from opcode word + 2 additional args = 3 words, not 4 → decLen
decode' 0xfd v xs = decLen <$> makeInstIIDD PUTW (high:low:xs) where Tuple high low = getNibbles v -- XXX: high:low or low:high?
decode' 0x7e v xs = decLen <$> makeInstIIDD GETW (high:low:xs) where Tuple high low = getNibbles v
decode' 0xfe v xs = makeInstI PENTER (v:xs)
-- incLen, so escape code + opcode are consumed in addition to args
decode' 0x7f opcode xs = incLen <$> decodeSecondary opcode xs
-- long primary instruction with FFh prefix. this works iff every primary
-- instruction has at least one argument (they do)
decode' 0xff opcode xs = incLen <$> decodePrimary opcode xs
decode' high low xs = decodePrimary opcode (arg1:xs)
	where Tuple opcode arg1 = (decodePrimaryOpcode high low)

-- | Modify the consumed length of a decoded instruction
modifyLen :: (Consumed -> Consumed) -> Tuple Instruction Consumed -> Tuple Instruction Consumed
modifyLen f (Tuple inst len) = Tuple inst (f len)

incLen = modifyLen ((+) 1)
decLen = modifyLen (flip (-) $ 1)

-- | Get upper/lower nibble (4 bits) of the 8 bit word `v`
getNibbles :: Word8 -> Tuple Word4 Word4
getNibbles v = Tuple high low
	where
		high = (and (shr v 4) 0xf)
		low = (and v 0xf)

-- Decode first Word16, split into two Word8, of primary instruction into opcode and argument
decodePrimaryOpcode :: Word8 -> Word8 -> Tuple Opcode Word16
decodePrimaryOpcode high low = Tuple opcode arg1
    where
		-- Mask high/low word according to sysdoc section 2.3 page 1. i is
		-- opcode bit, d is data address bit. Having one d bit at the high
		-- byte’s msb is intentional, see sysdoc section 2.1 page 36 (“Trick
		-- 1”). This way small global and local addresses can be encoded here.
		--   high      low
		-- diiiiidd dddddddd
        opcode = high `and` 0x7c
        arg1 = ((high `and` 0x83) `shl` 8) `or` low

-- Decode primary instruction, based on decoded opcode. The first argument
-- (part of opcode word16) must be the head of @xs
decodePrimary :: Opcode -> List Word16 -> DecodeResult
decodePrimary 0x00 = makeInstI LN
decodePrimary 0x04 = makeInstI LN1
decodePrimary 0x08 = makeInstDD MOV
decodePrimary 0x0c = makeInstD INC1
decodePrimary 0x10 = makeInstD DEC1
decodePrimary 0x14 = makeInstDD INC
decodePrimary 0x18 = makeInstDD DEC
decodePrimary 0x1c = makeInstDDD ADD
decodePrimary 0x20 = makeInstDDD SUB
decodePrimary 0x24 = makeInstD CLEAR
decodePrimary 0x28 = makeInstD TEST
decodePrimary 0x2c = makeInstDD EQU
decodePrimary 0x30 = makeInstDD LSEQ
decodePrimary 0x34 = makeInstDD FMOV
decodePrimary 0x38 = makeInstDDD FADD
decodePrimary 0x3c = makeInstDDD FSUB
decodePrimary 0x40 = makeInstDDD FMUL
decodePrimary 0x44 = makeInstDDD FDIV
decodePrimary 0x48 = makeInstDD FLSEQ
decodePrimary 0x4c = makeInstDD TMOV
decodePrimary 0x50 = makeInstDD TEQU
decodePrimary 0x54 = makeInstDD ULSEQ
decodePrimary 0x58 = makeInstDD DSACC
decodePrimary 0x5c = makeInstDD REF
decodePrimary 0x60 = makeInstIIDDD SUBS
decodePrimary 0x64 = makeInstDID SEL
decodePrimary 0x68 = makeInstD PPV
decodePrimary 0x6c = makeInstD PP
decodePrimary 0x70 = makeInstC B
decodePrimary 0x74 = makeInstC B1
decodePrimary 0x78 = makeInstM CALL
decodePrimary x = const $ makeOpcodeUnknown x

-- Decode secondary instruction, based on opcode (7fxxh) and tail
decodeSecondary :: Opcode -> List Word16 -> DecodeResult
decodeSecondary 0x00 = makeInst0 RTN
decodeSecondary 0x01 = makeInst0 RTNT
decodeSecondary 0x02 = makeInst0 RTNF
--decodeSecondary 0x03 = makeInst* ? -- nonexistent in 1.8
decodeSecondary 0x04 = makeInst0 STOP
decodeSecondary 0x05 = makeInstC GOSUB
decodeSecondary 0x06 = makeInst0 KE
decodeSecondary 0x07 = makeInst0 GORET
decodeSecondary 0x08 = makeInstDD BCRD
decodeSecondary 0x09 = makeInstDD CRD
--decodeSecondary 0x0a = makeInstDD ECWR -- original sysdoc
decodeSecondary 0x0a = makeInstDDD ECWR
decodeSecondary 0x0b = makeInstDDD CWR
decodeSecondary 0x0c = makeInstDD CTT
decodeSecondary 0x0d = makeInstDDD GETC
decodeSecondary 0x0e = makeInstDDD FNONBL
decodeSecondary 0x0f = makeInstDD DREM256
decodeSecondary 0x10 = makeInstDD AMUL256
--decodeSecondary 0x11 = makeInst* ?
decodeSecondary 0x12 = makeInstD ISDIG
decodeSecondary 0x13 = makeInstD ISLD
decodeSecondary 0x14 = makeInstD ISLCAS
decodeSecondary 0x15 = makeInstD ISUCAS
decodeSecondary 0x16 = makeInstDDD GADDR
decodeSecondary 0x17 = makeInstDDD GCADDR
decodeSecondary 0x18 = makeInstD ISSHA
decodeSecondary 0x19 = makeInst0 SYSG
decodeSecondary 0x1a = makeInst0 GETTAB
decodeSecondary 0x1b = makeInst0 PUTTAB
decodeSecondary 0x1c = makeInst0 ERTAB
decodeSecondary 0x1d = makeInstM EXEC
decodeSecondary 0x1e = makeInstM PPROC
decodeSecondary 0x1f = makeInstD PCALL
decodeSecondary 0x20 = makeInstDI BRCOMP
decodeSecondary 0x21 = makeInstIDD MOVXX
decodeSecondary 0x22 = makeInstIDD ALIAS
decodeSecondary 0x23 = makeInstID MOVII
decodeSecondary 0x24 = makeInstDD FEQU
decodeSecondary 0x25 = makeInstDD TLSEQ
decodeSecondary 0x26 = makeInstDD FNEG
decodeSecondary 0x27 = makeInstDD NEG
decodeSecondary 0x28 = makeInstDDD IMULT
decodeSecondary 0x29 = makeInstDDD MUL
decodeSecondary 0x2a = makeInstDDD DIV
decodeSecondary 0x2b = makeInstDDD MOD
decodeSecondary 0x2c = makeInstDDD ITSUB
decodeSecondary 0x2d = makeInstDDD ITRPL
decodeSecondary 0x2e = makeInstDD DECOD
decodeSecondary 0x2f = makeInstDD ENCOD
decodeSecondary 0x30 = makeInstDDD SUBT1
decodeSecondary 0x31 = makeInstDDDD SUBTFT
decodeSecondary 0x32 = makeInstDDD SUBTF
decodeSecondary 0x33 = makeInstDDD REPLAC
decodeSecondary 0x34 = makeInstDD CAT
decodeSecondary 0x35 = makeInstDD TLEN
decodeSecondary 0x36 = makeInstDDD POS
decodeSecondary 0x37 = makeInstDDDD POSF
decodeSecondary 0x38 = makeInstDDDDD POSFT
decodeSecondary 0x39 = makeInstDDDDDDD STRANL
decodeSecondary 0x3a = makeInstDDDDD POSIF
--decodeSecondary 0x3b = makeInst ?
decodeSecondary 0x3c = makeInstD OUT
decodeSecondary 0x3d = makeInstD COUT
decodeSecondary 0x3e = makeInstDD OUTF
decodeSecondary 0x3f = makeInstDDD OUTFT
decodeSecondary 0x40 = makeInstD INCHAR
decodeSecondary 0x41 = makeInstD INCETY
decodeSecondary 0x42 = makeInstD PAUSE
decodeSecondary 0x43 = makeInstDD GCPOS
decodeSecondary 0x44 = makeInstDD CATINP
decodeSecondary 0x45 = makeInstD NILDS
decodeSecondary 0x46 = makeInstDD DSCOPY
decodeSecondary 0x47 = makeInstD DSFORG
decodeSecondary 0x48 = makeInstDD DSWTYP
decodeSecondary 0x49 = makeInstDD DSRTYP
decodeSecondary 0x4a = makeInstDD DSHEAP
decodeSecondary 0x4b = makeInst0 ESTOP
decodeSecondary 0x4c = makeInst0 DSTOP
--decodeSecondary 0x4d = makeInst0 SETERR -- original sysdoc
decodeSecondary 0x4d = makeInstD SETERR
decodeSecondary 0x4e = makeInst0 ISERR
decodeSecondary 0x4f = makeInst0 CLRERR
decodeSecondary 0x50 = makeInstDD RPCB
--decodeSecondary 0x51 = makeInstDD WPCB -- original sysdoc
decodeSecondary 0x51 = makeInstDDD INFOPW
decodeSecondary 0x52 = makeInstDD TWCPU
decodeSecondary 0x53 = makeInstDD ROTATE
--decodeSecondary 0x54 = makeInst0 XREW -- original sysdoc
decodeSecondary 0x54 = makeInstDDDD CONTRL
--decodeSecondary 0x55 = makeInstDDD XWRITE
decodeSecondary 0x55 = makeInstDDDDD BLKOUT
--decodeSecondary 0x56 = makeInstDDD XREAD
decodeSecondary 0x56 = makeInstDDDDD BLKIN
--decodeSecondary 0x57 = makeInstD XSKIP
decodeSecondary 0x57 = makeInstDDD NXTDSP
--decodeSecondary 0x58 = makeInstD XERR
decodeSecondary 0x58 = makeInstDDD DSPAGS
decodeSecondary 0x59 = makeInstDD STORAGE
--decodeSecondary 0x5a = makeInst0 SHUTUP
decodeSecondary 0x5a = makeInstD SYSOP
decodeSecondary 0x5b = makeInst0 ARITS
decodeSecondary 0x5c = makeInst0 ARITU
decodeSecondary 0x5d = makeInstD HPSIZE
decodeSecondary 0x5e = makeInst0 GARB
--decodeSecondary 0x5f = makeInstD CHAEX
decodeSecondary 0x5f = makeInstDDDC TPBEGIN -- XXX: not sure how many bytes the last arg has
decodeSecondary 0x60 = makeInstDDD FSLD
decodeSecondary 0x61 = makeInstDD GEXP
decodeSecondary 0x62 = makeInstDD SEXP
decodeSecondary 0x63 = makeInstDD FLOOR
decodeSecondary 0x64 = makeInstDDD RTSUB
decodeSecondary 0x65 = makeInstDDD RTRPL
decodeSecondary 0x66 = makeInstDD CLOCK
decodeSecondary 0x67 = makeInstD SETNOW
decodeSecondary 0x68 = makeInstDDD TRPCB
decodeSecondary 0x69 = makeInstDDD TWPCB
decodeSecondary 0x6a = makeInstDD TCPU
decodeSecondary 0x6b = makeInstDD TSTAT
decodeSecondary 0x6c = makeInstD ACT
decodeSecondary 0x6d = makeInstD DEACT
decodeSecondary 0x6e = makeInstD THALT
decodeSecondary 0x6f = makeInstDD TBEG
decodeSecondary 0x70 = makeInstD TEND
decodeSecondary 0x71 = makeInstDDDD SEND
decodeSecondary 0x72 = makeInstDDD WAIT
decodeSecondary 0x73 = makeInstDDDD SWCALL
--decodeSecondary 0x74 = makeInstD CDBINT
decodeSecondary 0x74 = makeInstDD CDBINT
--decodeSecondary 0x75 = makeInstD CDBTXT
decodeSecondary 0x75 = makeInstDD CDBTXT
decodeSecondary 0x76 = makeInstD PNACT
decodeSecondary 0x77 = makeInstDDD PW
decodeSecondary 0x78 = makeInstDDD GW
decodeSecondary 0x79 = makeInstDDD XOR
decodeSecondary 0x7a = makeInstDDDD PPCALL
decodeSecondary 0x7b = makeInstD EXTASK
decodeSecondary 0x7c = makeInstDDD AND
decodeSecondary 0x7d = makeInstDDD OR
decodeSecondary 0x7e = makeInstD SESSION
decodeSecondary 0x7f = makeInstDDDDD SENDFT
decodeSecondary 0x80 = makeInstD DEFCOL
decodeSecondary 0x81 = makeInstDD ID
decodeSecondary x = const $ makeOpcodeUnknown x

makeInputTooShort n = Left $ InputTooShort n
makeOpcodeUnknown x = Left $ UnknownOpcode x

-- Instruction without argument
makeInst0 f _ = Right $ Tuple (Instruction0 f) 0

makeInstI f (arg1:_) = Right $ Tuple (InstructionI f arg1) 1
makeInstI _ _ = makeInputTooShort 1

makeInstM f (arg1:_) = Right $ Tuple (InstructionM f arg1) 1
makeInstM _ _ = makeInputTooShort 1

makeInstC f (arg1:_) = Right $ Tuple (InstructionC f arg1) 1
makeInstC _ _ = makeInputTooShort 1

makeInstD f (arg1:_) = Right $ Tuple (InstructionD f (decodeDataAddress arg1)) 1
makeInstD _ _ = makeInputTooShort 1

makeInstID f (arg1:arg2:_) = Right $ Tuple (InstructionID f arg1 (decodeDataAddress arg2)) 2
makeInstID _ _ = makeInputTooShort 2

makeInstDI f (arg1:arg2:_) = Right $ Tuple (InstructionDI f (decodeDataAddress arg1) arg2) 2
makeInstDI _ _ = makeInputTooShort 2

makeInstDD f (arg1:arg2:_) = Right $ Tuple (InstructionDD f (decodeDataAddress arg1) (decodeDataAddress arg2)) 2
makeInstDD _ _ = makeInputTooShort 2

makeInstIDD f (arg1:arg2:arg3:_) = Right $ Tuple (InstructionIDD f arg1 (decodeDataAddress arg2) (decodeDataAddress arg3)) 3
makeInstIDD _ _ = makeInputTooShort 3

makeInstDID f (arg1:arg2:arg3:_) = Right $ Tuple (InstructionDID f (decodeDataAddress arg1) arg2 (decodeDataAddress arg3)) 3
makeInstDID _ _ = makeInputTooShort 3

makeInstDDD f (arg1:arg2:arg3:_) = Right $ Tuple (InstructionDDD f (decodeDataAddress arg1) (decodeDataAddress arg2) (decodeDataAddress arg3)) 3
makeInstDDD _ _ = makeInputTooShort 3

makeInstIIDD f (arg1:arg2:arg3:arg4:_) = Right $ Tuple (InstructionIIDD f arg1 arg2 (decodeDataAddress arg3) (decodeDataAddress arg4)) 4
makeInstIIDD _ _ = makeInputTooShort 4

makeInstDDDD f (arg1:arg2:arg3:arg4:_) = Right $ Tuple (InstructionDDDD f (decodeDataAddress arg1) (decodeDataAddress arg2) (decodeDataAddress arg3) (decodeDataAddress arg4)) 4
makeInstDDDD _ _ = makeInputTooShort 4

makeInstDDDC f (arg1:arg2:arg3:arg4:_) = Right $ Tuple (InstructionDDDC f (decodeDataAddress arg1) (decodeDataAddress arg2) (decodeDataAddress arg3) (decodeCodeAddress arg4)) 4
makeInstDDDC _ _ = makeInputTooShort 4

makeInstDDDDD f (arg1:arg2:arg3:arg4:arg5:_) = Right $ Tuple (InstructionDDDDD f (decodeDataAddress arg1) (decodeDataAddress arg2) (decodeDataAddress arg3) (decodeDataAddress arg4) (decodeDataAddress arg5)) 5
makeInstDDDDD _ _ = makeInputTooShort 5

makeInstDDDDDDD f (arg1:arg2:arg3:arg4:arg5:arg6:arg7:_) = Right $ Tuple (InstructionDDDDDDD f (decodeDataAddress arg1) (decodeDataAddress arg2) (decodeDataAddress arg3) (decodeDataAddress arg4) (decodeDataAddress arg5) (decodeDataAddress arg6) (decodeDataAddress arg7)) 7
makeInstDDDDDDD _ _ = makeInputTooShort 7

makeInstIIDDD f (arg1:arg2:arg3:arg4:arg5:_) = Right $ Tuple (InstructionIIDDD f arg1 arg2 (decodeDataAddress arg3) (decodeDataAddress arg4) (decodeDataAddress arg5)) 5
makeInstIIDDD _ _ = makeInputTooShort 5

decodeDataAddress :: Word16 -> DataAddress
decodeDataAddress v | msb16 v == 0 = GlobalDataAddress v
-- else: msb is one and lsb is zoro
decodeDataAddress v | lsb v == 0 = LocalDataAddress ((v `and` 0x7fff) `div` 2)
-- else: msb one lsb one:
decodeDataAddress v | otherwise = LocalDataAddressRef ((v `and` 0x7fff) `div` 2)

msb16 x = (x `shr` 15) `and` 1
lsb x = x `and` 1

decodeCodeAddress :: Word16 -> CodeAddress
decodeCodeAddress v = v

