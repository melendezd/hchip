{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TupleSections #-}

module Chip8.ROM where

import Chip8.Types
import Control.Lens hiding (index)
import Data.Array hiding (index)
import Data.Bits
import Data.ByteString (index)
import qualified Data.ByteString as BS
import Data.List (intersperse)
import Data.Word
import Numeric (showHex)
import System.IO.Unsafe (unsafePerformIO)
import System.Random (genWord8)

--------------------------------------------------------------------------------
-- Machine functions -----------------------------------------------------------
--------------------------------------------------------------------------------

currentOp :: Machine -> Either String Instruction
currentOp !m = readOp ((m ^. cpu . memory) ! (1 * pc)) ((m ^. cpu . memory) ! (1 * pc + 1))
  where
    pc = m ^. cpu . registerFile . regPC

updateMachine :: Machine -> Either String Machine
updateMachine !m = do
  let !m' = m & cpu . registerFile . regPC +~ 2
  --let !_ = unsafePerformIO $ print "printing array"
  --let !_ = unsafePerformIO $ mapM_ print (assocs $ m'^.cpu.registerFile.regV)
  --let !_ = unsafePerformIO $ print "done printing array"
  applyOp <$> currentOp m <*> pure m'

--------------------------------------------------------------------------------
-- bytes to instructions -------------------------------------------------------
--------------------------------------------------------------------------------

-- Copies a ROM from a ByteString directly into the CPU's memory, starting at 0x200
loadROM :: BS.ByteString -> Machine -> Machine
loadROM bytes !m = m & over (cpu . memory) (// zip (range (0x200, fromIntegral $ 0x200 + BS.length bytes -1)) (BS.unpack bytes))

-- Converts two consecutive bytes into an instruction
readOp :: Word8 -> Word8 -> Either String Instruction
readOp b1 b0 = dto (b1 `div` 0x10) (b1 `mod` 0x10) (b0 `div` 0x10) (b0 `mod` 0x10)

-- utility functions for dto ---------------------------------------------------
full :: Word8 -> Word8 -> Word8 -> Word8 -> Word16
full n3 n2 n1 n0 = n0' + 0x10 * n1' + 0x100 * n2' + 0x1000 * n3'
  where
    n0' = fromIntegral n0 :: Word16
    n1' = fromIntegral n1 :: Word16
    n2' = fromIntegral n2 :: Word16
    n3' = fromIntegral n3 :: Word16

addr :: Word8 -> Word8 -> Word8 -> Word16
addr n2 n1 n0 = n0' + 0x10 * n1' + 0x100 * n2'
  where
    n0' = fromIntegral n0 :: Word16
    n1' = fromIntegral n1 :: Word16
    n2' = fromIntegral n2 :: Word16

byte :: Word8 -> Word8 -> Word8
byte n1 n0 = n0 + 0x10 * n1

--------------------------------------------------------------------------------

-- Converts a sequence of four hex digits into an instruction
dto :: Word8 -> Word8 -> Word8 -> Word8 -> Either String Instruction
--dto 0x0 0x0 0x0 0x0  = Right   NOOP
dto 0x0 0x0 0xE 0x0 = Right CLS
dto 0x0 0x0 0xE 0xE = Right RET
--dto 0x0  n2  n1  n0  = Right $ SYS                   (addr n2 n1 n0)
dto 0x0 n2 n1 n0 = Right NOOP
dto 0x1 n2 n1 n0 = Right $ JP (addr n2 n1 n0)
dto 0x2 n2 n1 n0 = Right $ CALL (addr n2 n1 n0)
dto 0x3 x k1 k0 = Right $ SEi x (byte k1 k0)
dto 0x4 x k1 k0 = Right $ SNEi x (byte k1 k0)
dto 0x5 x y 0x0 = Right $ SE x y
dto 0x6 x k1 k0 = Right $ LDi x (byte k1 k0)
dto 0x7 x k1 k0 = Right $ ADDi x (byte k1 k0)
dto 0x8 x y 0x0 = Right $ LD x y
dto 0x8 x y 0x1 = Right $ OR x y
dto 0x8 x y 0x2 = Right $ AND x y
dto 0x8 x y 0x3 = Right $ XOR x y
dto 0x8 x y 0x4 = Right $ ADD x y
dto 0x8 x y 0x5 = Right $ SUB x y
dto 0x8 x y 0x6 = Right $ SHR x y
dto 0x8 x y 0x7 = Right $ SUBN x y
dto 0x8 x y 0xE = Right $ SHL x y
dto 0x9 x y 0x0 = Right $ SNE x y
dto 0xA n2 n1 n0 = Right $ LDI (addr n2 n1 n0)
dto 0xB n2 n1 n0 = Right $ JPV0 (addr n2 n1 n0)
dto 0xC x k1 k0 = Right $ RND x (byte k1 k0)
dto 0xD x y n = Right $ DRW x y n
dto 0xE x 0x9 0xE = Right $ SKP x
dto 0xE x 0xA 0x1 = Right $ SKNP x
dto 0xF x 0x0 0x7 = Right $ LDRD x
dto 0xF x 0x0 0xA = Right $ LDRK x
dto 0xF x 0x1 0x5 = Right $ LDDR x
dto 0xF x 0x1 0x8 = Right $ LDSR x
dto 0xF x 0x1 0xE = Right $ ADDIR x
dto 0xF x 0x2 0x9 = Right $ LDIS x
dto 0xF x 0x3 0x3 = Right $ LDB x
dto 0xF x 0x5 0x5 = Right $ LDIR x
dto 0xF x 0x6 0x5 = Right $ LDRI x
dto a b c d = Left $ "Invalid instruction: " ++ foldr1 (++) (intersperse " " $ fmap (($ []) . showHex) [a, b, c, d])

compose :: [a -> a] -> a -> a
compose = foldr1 (.)

applyOp :: Instruction -> Machine -> Machine
applyOp CLS m =
  m & over display (// zip allPoints (repeat 0))
    & drawFlag .~ True
  where
    allPoints = range $ bounds (m ^. display)
applyOp (DRW regXInd regYInd size) m =
  m & over display (updateInPlace xor spriteAssocsOnDisplay)
    & cpu . registerFile . regV . ix 0xF .~ (if collision then 1 else 0)
    & drawFlag .~ True
  where
    memStart = m ^. cpu . registerFile . regI
    bytes = [(m ^. cpu . memory) ! k | k <- [memStart .. memStart + fromIntegral size - 1]]
    bits byte = take 8 . tail $ fmap (`mod` 2) (iterate (`rotateL` 1) byte)
    toAssocsRow byte = zip [0 ..] (bits byte) -- list of (xVal, bit)
    toAssocs bytes = (smashCoord . onVals toAssocsRow) =<< mkYVals bytes
      where
        onVals f (a, b) = (a, f b)
        smashCoord (y, xAssocs) = fmap (\(x, a) -> ((x, y), a)) xAssocs
        mkYVals = zip [0 ..]
    sprite = array ((0, 0), (8 -1, size -1)) (toAssocs bytes)
    x = (m ^. cpu . registerFile . regV) ! regXInd
    y = (m ^. cpu . registerFile . regV) ! regYInd
    spriteAssocsOnDisplay = fmap (onIndices (wrap . translate)) (assocs sprite)
      where
        onIndices f (a, b) = (f a, b)
        translate (a, b) = (a + x, b + y)
        wrap (a, b) = (a `mod` displayWidth, b `mod` displayHeight)

    destXVals = fmap (`mod` displayWidth) [x .. x + 7]
    destYVals = fmap (`mod` displayHeight) [y .. y + size -1]
    spriteRows = fmap bits bytes
    !collision = or $ fmap spriteMatchesDisplay spriteIndices
      where
        spriteMatchesDisplay pos = m ^. display . ix (wrap . translate $ pos) .&. (sprite ! pos) == 1
        spriteIndices = Data.Array.indices sprite
        onIndices f (a, b) = (f a, b)
        translate (a, b) = (a + x, b + y)
        wrap (a, b) = (a `mod` displayWidth, b `mod` displayHeight)
-- !collision = or $ fmap (\(x, y) -> ((m^.display) ! (x,y)) == 1)
--(range ( (x,y), (min (x+7) displayWidth, min (y+size-1) displayHeight) ) )

-- Spin wait until key is pressed
--applyOp (LDRK regInd) m =
--  case m ^. key of
--    Nothing -> m & cpu . registerFile . regPC -~ 2
--    Just k -> m & cpu . registerFile . regV . ix regInd .~ fromIntegral (fromEnum k)
--    & drawFlag .~ False
applyOp (LDRK regInd) m = m -- 
    & drawFlag .~ False
applyOp (SKP regSrcInd) m =
  m
    & cpu . registerFile . regPC
      +~ (if m ^. key == Just (toEnum . fromIntegral $ regSrc) then 2 else 0)
    & drawFlag .~ False
  where
    regSrc = m ^. cpu . registerFile . regV . ix regSrcInd
applyOp (SKNP regSrcInd) m =
  m & cpu . registerFile . regPC
    +~ (if m ^. key == Just (toEnum (fromIntegral regSrc)) then 0 else 2)
      & drawFlag .~ False
  where
    regSrc = m ^. cpu . registerFile . regV . ix regSrcInd
applyOp op m =
  m & over cpu (applyOpCPU op)
    & drawFlag .~ False

applyOpCPU :: Instruction -> CPU -> CPU
applyOpCPU RET cpu = cpu & (registerFile . regPC .~ dest) & over stack tail
  where
    dest = head (cpu ^. stack)
applyOpCPU (SYS dest) cpu =
  cpu & registerFile . regPC .~ dest
    & over stack (src :)
  where
    src = cpu ^. registerFile . regPC
applyOpCPU (JP dest) cpu = cpu & registerFile . regPC .~ dest
applyOpCPU (CALL dest) cpu = cpu & over stack (pc :) & (registerFile . regPC .~ dest)
  where
    pc = cpu ^. registerFile . regPC
applyOpCPU (SEi regInd imm) cpu = cpu & if reg == imm then registerFile . regPC +~ 2 else id
  where
    reg = cpu ^. registerFile . regV . ix regInd
applyOpCPU (SNEi regInd imm) cpu = cpu & if reg /= imm then registerFile . regPC +~ 2 else id
  where
    reg = (cpu ^. registerFile . regV) ! regInd
applyOpCPU (SE regInd1 regInd2) cpu = cpu & if reg1 == reg2 then registerFile . regPC +~ 2 else id
  where
    reg1 = (cpu ^. registerFile . regV) ! regInd1
    reg2 = (cpu ^. registerFile . regV) ! regInd2
applyOpCPU (LDi regInd imm) cpu = cpu & over (registerFile . regV) (// [(regInd, imm)])
applyOpCPU (ADDi regInd imm) cpu = cpu & over (registerFile . regV) (updateBy [(regInd, (+ imm))])
applyOpCPU (LD regIndDest regIndSrc) cpu = cpu & over (registerFile . regV) (// [(regIndDest, regSrc)])
  where
    regSrc = (cpu ^. registerFile . regV) ! regIndSrc
applyOpCPU (OR regIndDest regIndSrc) cpu = cpu & registerFile . regV . ix regIndDest .~ regDest .|. regSrc
  where
    regSrc = cpu ^. registerFile . regV . ix regIndSrc
    regDest = cpu ^. registerFile . regV . ix regIndDest
applyOpCPU (AND regIndDest regIndSrc) cpu = cpu & registerFile . regV . ix regIndDest .~ regDest .&. regSrc
  where
    regSrc = cpu ^. registerFile . regV . ix regIndSrc
    regDest = cpu ^. registerFile . regV . ix regIndDest
applyOpCPU (XOR regIndDest regIndSrc) cpu = cpu & registerFile . regV . ix regIndDest .~ regDest `xor` regSrc
  where
    regSrc = cpu ^. registerFile . regV . ix regIndSrc
    regDest = cpu ^. registerFile . regV . ix regIndDest
applyOpCPU (ADD regIndDest regIndSrc) cpu =
  cpu
    & over (registerFile . regV) (// [(regIndDest, sum), (0xF, carry)])
  where
    regSrc = (cpu ^. registerFile . regV) ! regIndSrc
    regDest = (cpu ^. registerFile . regV) ! regIndDest
    sum = regSrc + regDest
    carry = if sum < regSrc then 1 else 0
applyOpCPU (SUB regIndDest regIndSrc) cpu =
  cpu & registerFile . regV . ix regIndDest .~ diff
    & registerFile . regV . ix 0xF .~ borrow
  where
    regSrc = (cpu ^. registerFile . regV) ! regIndSrc
    regDest = (cpu ^. registerFile . regV) ! regIndDest
    diff = regDest - regSrc
    borrow = if regSrc > regDest then 0 else 1
applyOpCPU (SHR regIndDest regIndSrc) cpu =
  cpu
    & over (registerFile . regV) (// [(regIndDest, result), (0xF, lsb)])
  where
    regSrc = (cpu ^. registerFile . regV) ! regIndSrc
    regDest = (cpu ^. registerFile . regV) ! regIndDest
    result = shift regSrc (-1)
    lsb = regSrc `mod` 2
applyOpCPU (SUBN regIndDest regIndSrc) cpu = applyOpCPU (SUB regIndSrc regIndDest) cpu
applyOpCPU (SHL regIndDest regIndSrc) cpu =
  cpu
    & over (registerFile . regV) (// [(regIndDest, result), (0xF, msb)])
  where
    regSrc = (cpu ^. registerFile . regV) ! regIndSrc
    regDest = (cpu ^. registerFile . regV) ! regIndDest
    result = shift regSrc 1
    msb = rotate regSrc 1 `mod` 2
applyOpCPU (SNE regInd1 regInd2) cpu = cpu & if reg1 /= reg2 then registerFile . regPC +~ 2 else id
  where
    reg1 = (cpu ^. registerFile . regV) ! regInd1
    reg2 = (cpu ^. registerFile . regV) ! regInd2
applyOpCPU (LDI val) cpu = cpu & registerFile . regI .~ val
applyOpCPU (JPV0 dest) cpu = cpu & registerFile . regPC .~ dest + fromIntegral offset
  where
    offset = (cpu ^. registerFile . regV) ! 0
applyOpCPU (LDRD regDestInd) cpu = applyOpCPU (LDi regDestInd val) cpu
  where
    val = cpu ^. registerFile . regDT
applyOpCPU (LDDR regSrcInd) cpu = cpu & registerFile . regDT .~ regSrc
  where
    regSrc = (cpu ^. registerFile . regV) ! regSrcInd
applyOpCPU (LDSR regSrcInd) cpu = cpu & registerFile . regST .~ regSrc
  where
    regSrc = (cpu ^. registerFile . regV) ! regSrcInd
applyOpCPU (ADDIR regSrcInd) cpu = applyOpCPU (LDI val) cpu
  where
    val = cpu ^. registerFile . regI + getRegV cpu regSrcInd
applyOpCPU (LDIS regSrcInd) cpu = applyOpCPU (LDI loc) cpu
  where
    regSrc = getRegV cpu regSrcInd
    loc = 5 * regSrc
applyOpCPU (LDB regSrcInd) cpu = cpu & over memory (// [(i, hundreds), (i + 1, tens), (i + 2, ones)])
  where
    i = cpu ^. registerFile . regI
    regSrc = cpu ^. registerFile . regV . ix regSrcInd
    hundreds = regSrc `div` 100
    tens = regSrc `div` 10 `mod` 10
    ones = regSrc `mod` 10
applyOpCPU (LDIR regEndInd) cpu = cpu & over memory (// replacements)
  where
    i = cpu ^. registerFile . regI
    replacements = zip [i .. i + fromIntegral regEndInd] ((\x -> cpu ^. registerFile . regV . ix x) <$> [0 .. regEndInd])
applyOpCPU (LDRI regEndInd) cpu = cpu & over (registerFile . regV) (// replacements)
  where
    i = cpu ^. registerFile . regI
    replacements = zip [0 .. regEndInd] ((\x -> cpu ^. memory . ix x) <$> [i .. i + fromIntegral regEndInd])
applyOpCPU (RND regSrcInd val) cpu =
  cpu & regFile . regV . ix regSrcInd .~ (randVal .&. val)
    & seed .~ g
  where
    (randVal, g) = genWord8 (cpu ^. seed)
applyOpCPU NOOP cpu = cpu

--------------------------------------------------------------------------------
-- utility functions -----------------------------------------------------------
--------------------------------------------------------------------------------

regFile = registerFile

-- For each (i, f) in pairs, replaces arr[i] with f arr[i]
updateBy :: (Ix i) => [(i, a -> a)] -> Array i a -> Array i a
updateBy pairs arr = arr // valPairs
  where
    valPairs = fmap (\(i, f) -> (i, f (arr ! i))) pairs

updateInPlace :: (Ix i) => (a -> a -> a) -> [(i, a)] -> Array i a -> Array i a
updateInPlace f assocs = updateBy (fmap (\(i, a) -> (i, f a)) assocs)
