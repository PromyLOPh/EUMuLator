-- Assembly/disassembly

module Asm (toAsm) where

import Prelude ((<>), show)
import Data.List
import Data.Int

import InstructionSet

-- | Convert Instruction to assembly mnemonic string + arguments
toAsm :: Instruction -> List String
toAsm (Instruction0 op) = show op:Nil
toAsm (InstructionI op imm) = show op:immediate imm:Nil
toAsm (InstructionC op code) = show op:codeAddress code:Nil
toAsm (InstructionM op m) = show op:modno m:Nil
toAsm (InstructionD op d) = show op:dataAddress d:Nil
toAsm (InstructionDD op d1 d2) = show op:dataAddress d1:dataAddress d2:Nil
toAsm (InstructionID op imm d) = show op:immediate imm:dataAddress d:Nil
toAsm (InstructionDI op d imm) = show op:dataAddress d:immediate imm:Nil
toAsm (InstructionDDD op d1 d2 d3) = show op:dataAddress d1:dataAddress d2:dataAddress d3:Nil
toAsm (InstructionDID op d1 imm d2) = show op:dataAddress d1:immediate imm:dataAddress d2:Nil
toAsm (InstructionIDD op imm d1 d2) = show op:immediate imm:dataAddress d1:dataAddress d2:Nil
toAsm (InstructionIIDD op i1 i2 d1 d2) = show op:immediate i1:immediate i2:dataAddress d1:dataAddress d2:Nil
toAsm (InstructionDDDD op d1 d2 d3 d4) = show op:dataAddress d1:dataAddress d2:dataAddress d3:dataAddress d4:Nil
toAsm (InstructionDDDC op d1 d2 d3 c) = show op:dataAddress d1:dataAddress d2:dataAddress d3:codeAddress c:Nil
toAsm (InstructionIIDDD op i1 i2 d1 d2 d3) = show op:immediate i1:immediate i2:dataAddress d1:dataAddress d2:dataAddress d3:Nil
toAsm (InstructionDDDDD op d1 d2 d3 d4 d5) = show op:dataAddress d1:dataAddress d2:dataAddress d3:dataAddress d4:dataAddress d5:Nil
toAsm (InstructionDDDDDDD op d1 d2 d3 d4 d5 d6 d7) = show op:dataAddress d1:dataAddress d2:dataAddress d3:dataAddress d4:dataAddress d5:dataAddress d6:dataAddress d7:Nil

dataAddress :: DataAddress -> String
dataAddress (GlobalDataAddress a) = "*" <> (hex a)
dataAddress (LocalDataAddress a) = "=" <> (hex a)
dataAddress (LocalDataAddressRef a) = "<=" <> (hex a) <> ">"

codeAddress a = "$" <> (hex a)

immediate :: Immediate -> String
immediate v = "#" <> hex v

modno :: Module -> String
modno v = "&" <> hex v

hex :: Int -> String
hex v = toStringAs hexadecimal v <> "h"

