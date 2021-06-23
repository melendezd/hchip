module Main (main) where

import Chip8
import Chip8.Internal

import Test.Hspec

import Data.Array
import Data.Word

import Control.Lens
import Control.Lens.Operators

import qualified Data.ByteString as BS

import Control.Monad.IO.Class


rom = [0x8A, 0x30, 0xAA, 0xBC, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xD5, 0xA7 ]
testMachine = newMachine <&> over (cpu.memory) 
                                (// zip [0x200..] [0x8A, 0x30,  -- LD 0xA 0x3
                                                   0xAA, 0xBC,  -- LDI 0xABC
                                                   0x00, 0x00,
                                                   0x00, 0x00,
                                                   0x00, 0x00,
                                                   0x00, 0x00,
                                                   0x00, 0x00,
                                                   0x00, 0x00,
                                                   0x00, 0x00,
                                                   0xD5, 0xA7
                                                  ]) 

main :: IO ()
main = do
    husk <- newMachine
    tm <- testMachine
    hspec $ do
        describe "Chip8.ROM.currentOp" $ do
            it "gets the instruction at PC=0x200" $ 
                currentOp tm `shouldBe` Right (LD 0xA 0x3)
            it "gets the instruction at PC=0x202" $ 
                currentOp (tm & cpu.registerFile.regPC.~0x202) `shouldBe` Right (LDI 0xABC)
            it "gets the instruction at PC=0x212" $ 
                currentOp (tm & cpu.registerFile.regPC.~0x212) `shouldBe` Right (DRW 0x5 0xA 0x7)
        describe "Chip8.ROM.readOp" $ do
            it "reads LDi" $ 
                readOp 0x68 0x25 `shouldBe` Right (LDi 0x8 0x25)
            it "reads CALL" $ 
                readOp 0x2A 0xBC `shouldBe` Right (CALL 0xABC)
        describe "Chip8.ROM.loadROM" $ do
            it "load a ROM into memory" $ 
                loadROM (BS.pack rom) husk `shouldBe` tm
        describe "CLS" $ do
            it "clears the screen" $
                applyOp CLS (husk & over display ( // (map (\(i,x) -> (i,1)) $ assocs (husk^.display)) ))
                    `shouldBe` (husk & drawFlag .~ True)
        describe "RET" $ do
            let mRet = husk & cpu.stack .~ [500, 400, 300]
            let mRetAfter = applyOp RET mRet
            it "sets the PC to addr at top of stack" $
                mRetAfter^.cpu.registerFile.regPC `shouldBe` 500
            it "pops the address from the top of stack" $
                mRetAfter^.cpu.stack `shouldBe` [400, 300]
        describe "JP" $ do
            let mJPAfter = applyOp (JP 500) husk
            it "sets the PC to the specified address" $
                mJPAfter^.cpu.registerFile.regPC `shouldBe` 500
        describe "CALL" $ do
            let mCALL = husk & cpu.stack .~ [1, 2, 3, 4, 5]
                             & cpu.registerFile.regPC .~ 0x400
            let mCALL' = applyOp (CALL 0x300) mCALL
            it "sets the PC to the specified address" $
                mCALL'^.cpu.registerFile.regPC `shouldBe` 0x300
            it "pushes the PC to the top of the stack" $
                mCALL'^.cpu.stack `shouldBe` [0x400, 1, 2, 3, 4, 5]
        describe "SEi" $ do
            let mSEi   = husk & cpu.registerFile.regV.ix 4 .~ 5
                mSEi'  = applyOp (SEi 4 5) mSEi
                mSEi'' = applyOp (SEi 4 6) mSEi
            it "skips when equal" $
                mSEi'^.cpu.registerFile.regPC `shouldBe` 0x202
            it "doesn't skip when not equal" $
                mSEi''^.cpu.registerFile.regPC `shouldBe` 0x200
        describe "SNEi" $ do
            let m   = husk & cpu.registerFile.regV.ix 4 .~ 5
                m'  = applyOp (SNEi 4 5) m
                m'' = applyOp (SNEi 4 6) m
            it "skips when not equal" $
                m''^.cpu.registerFile.regPC `shouldBe` 0x202
            it "doesn't skip when equal" $
                m'^.cpu.registerFile.regPC `shouldBe` 0x200
        describe "SE" $ do
            let m   = husk & cpu.registerFile.regV.ix 4 .~ 4
                           & cpu.registerFile.regV.ix 5 .~ 4
                           & cpu.registerFile.regV.ix 6 .~ 6
                m'  = applyOp (SE 4 5) m
                m'' = applyOp (SE 4 6) m
            it "skips when equal" $
                m'^.cpu.registerFile.regPC `shouldBe` 0x202
            it "doesn't skip when not equal" $
                m''^.cpu.registerFile.regPC `shouldBe` 0x200
        describe "LDi" $ do
            let m = husk & cpu.registerFile.regV.ix 7 .~ 17
            it "sets the register" $
                (m^.cpu.registerFile.regV) ! 7 `shouldBe` 17
        describe "ADDi" $ do
            let m = husk & cpu.registerFile.regV.ix 5 .~ 6
                m' = applyOp (ADDi 5 3) m
            it "adds to the register" $
                m'^.cpu.registerFile.regV.ix 5 `shouldBe` 9
        describe "LD" $ do
            let m = husk & cpu.registerFile.regV.ix 5 .~ 6
                         & cpu.registerFile.regV.ix 0xA .~ 50
                m' = applyOp (LD 5 0xA) m
            it "loads into register" $
                m'^.cpu.registerFile.regV.ix 5 `shouldBe` 50
        describe "OR" $ do
            let m = husk & cpu.registerFile.regV.ix 5 .~ 6
                         & cpu.registerFile.regV.ix 0xA .~ 50
                m' = applyOp (OR 5 0xA) m
            it "computes OR correctly" $
                m'^.cpu.registerFile.regV.ix 5 `shouldBe` 54
        describe "AND" $ do
            let m = husk & cpu.registerFile.regV.ix 5 .~ 6
                         & cpu.registerFile.regV.ix 0xA .~ 50
                m' = applyOp (AND 5 0xA) m
            it "computes AND correctly" $
                m'^.cpu.registerFile.regV.ix 5 `shouldBe` 2
        describe "XOR" $ do
            let m = husk & cpu.registerFile.regV.ix 5 .~ 6
                         & cpu.registerFile.regV.ix 0xA .~ 50
                m' = applyOp (XOR 5 0xA) m
            it "computes XOR correctly" $
                m'^.cpu.registerFile.regV.ix 5 `shouldBe` 52
        describe "ADD" $ do
            let m = husk & cpu.registerFile.regV.ix 5 .~ 6
                         & cpu.registerFile.regV.ix 0xA .~ 50
                m' = applyOp (ADD 5 0xA) m
                n = husk & cpu.registerFile.regV.ix 5 .~ 255
                         & cpu.registerFile.regV.ix 0xA .~ 5
                n' = applyOp (ADD 5 0xA) n
            it "computes ADD carry correctly" $
                n'^.cpu.registerFile.regV.ix 0xF `shouldBe` 1
        describe "SUB" $ do
            let m = husk & cpu.registerFile.regV.ix 5 .~ 50
                         & cpu.registerFile.regV.ix 0xA .~ 6
                m' = applyOp (SUB 5 0xA) m
                m'' = applyOp (SUB 0xA 5) m
            it "computes SUB correctly" $
                m'^.cpu.registerFile.regV.ix 5 `shouldBe` 44
            it "computes SUB no borrow correctly" $
                m'^.cpu.registerFile.regV.ix 0xF `shouldBe` 1
            it "computes SUB borrow correctly" $
                m''^.cpu.registerFile.regV.ix 0xF `shouldBe` 0
        describe "LDB" $ do
            let m = husk & (cpu.registerFile.regI .~ 0x3e8) & applyOp (LDi 6 0x89) & applyOp (LDB 6)
            it "works" $
                (\x -> m^.cpu.memory.ix x) <$> [0x3e8..0x3e8+2] `shouldBe` [1, 3, 7]
        describe "LDRI" $ do
            let m  = husk & (cpu.registerFile.regI .~ 0x3e8) & over (cpu.memory) (// zip [0x3e8..] [5,6,7])
                m' = applyOp (LDRI 2) m
            it "works" $ do
                (\x -> m'^.cpu.registerFile.regV.ix x) <$> [0,1,2] `shouldBe` [5,6,7]


