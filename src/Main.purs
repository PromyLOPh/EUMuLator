module Main (main, fileToArrayView) where

import Prelude
import Effect (Effect, foreachE)
import Effect.Console (log)
import Data.List
import Data.List as L
import Data.Tuple
import Data.Int
import Data.String.Utils (length, repeat)
import Data.Maybe (fromJust, Maybe(..), maybe)
import Partial.Unsafe (unsafePartial)
import Node.FS.Sync (readFile)
import Node.Buffer (toArrayBuffer)
import Data.ArrayBuffer.Typed (whole, toArray, (!))
import Data.ArrayBuffer.Types (Uint16, ArrayView)
import Data.UInt (toInt)
import Data.Either (either)
import Control.Monad.Rec.Class
import Node.Process (argv)
import Data.Array ((!!))

import Decode
import Asm
import InstructionSet
import Machine

-- | Left-pad a string `s` to at least `num` characters with `pad`
lpad :: Int -> String -> String -> String
lpad num pad s = prefix <> s
	where
		prefix = case repeat (num-(length s)) pad of
			Just x -> x
			Nothing -> ""

-- | Right-pad a string `s` to at least `num` characters with `pad`
rpad :: Int -> String -> String -> String
rpad num pad s = s <> prefix
	where
		prefix = case repeat (num-(length s)) pad of
			Just x -> x
			Nothing -> ""

-- | Hex-format number `v`, adding padding to at least `len` characters
toHex :: Int -> Int -> String
toHex len v = lpad len "0" $ toStringAs hexadecimal v

hexdump :: List Int -> String
hexdump words = intercalate " " $ map (toHex 4) words

-- Format instruction with hexdump @width
formatInst :: Int -> Int -> Tuple (List Int) Instruction -> String
formatInst address width (Tuple words inst) = toHex 8 address <> " | " <> rpad width " " (hexdump words) <> " | " <> (rpad 10 " " mnemonic) <> "  " <> (intercalate ", " args)
	where
		asmInst = toAsm inst
		mnemonic = unsafePartial $ fromJust $ head asmInst
		args = unsafePartial $ fromJust $ tail asmInst

-- | Create ArrayView from a file `name`.
fileToArrayView :: String -> Effect (ArrayView Uint16)
fileToArrayView name = (readFile name) >>= toArrayBuffer >>= whole

logInst address = (log <<< (formatInst address (hexWords*hexWordWidth+hexWords*spaceWidth)))
	where
		hexWordWidth = 4
		hexWords = maxInstructionLen
		spaceWidth = 1

-- Take n words from an ArrayView starting at offset
takeView :: Int -> Int -> ArrayView Uint16 -> Effect (List Int)
takeView _ 0 _ = pure Nil
takeView offset n view = (view ! offset) >>= maybe (pure Nil) recurse
	where
		recurse v = ((:) (toInt v)) <$> (takeView (offset+1) (n-1) view)

-- Disassemble memory in @view starting at @startAddress
disasm :: Int -> ArrayView Uint16 -> Effect Unit
disasm startAddress view = tailRecM go startAddress
	where
		go address = do
			words <- takeView address maxInstructionLen view
			either invalid (continue address words) $ decode words
		invalid (InputTooShort x) = do
			log $ "input too short " <> show x
			pure $ Done unit
		invalid (UnknownOpcode x) = do
			log $ "unknown opcode " <> show x
			pure $ Done unit
		continue address words (Tuple inst len) = do
			logInst (address*wordsize) (Tuple (take len words) inst)
			pure $ Loop(address+len)
			
main :: Effect Unit
main = secondArg >>= (maybe argumentMissing doDisasm)
	where
		secondArg = (flip (!!) $ 2) <$> argv
		argumentMissing = log "Missing argument"
		doDisasm file = fileToArrayView file >>= disasm codeOffset

