{-# LANGUAGE TemplateHaskell #-}

module Chip8.Types (
    Machine(..), cpu, display, key, newMachine, drawFlag,
    CPU(..), registerFile, stack, memory, state, seed, newCPU,
    RegisterFile(..), regV, regI, regDT, regST, regPC,
    Instruction(..),
    Key(..),
    getRegV, onRegV, onRegV2,
    displayWidth, displayHeight
) where

import Data.Array
import System.Random
import Data.Word
import Control.Monad.IO.Class

import Control.Lens
import Control.Lens.TH


--------------------------------------------------------------------------------
-- types -----------------------------------------------------------------------
--------------------------------------------------------------------------------

data Key = Key0 | Key1 | Key2 | Key3 | Key4 | Key5 | Key6 | Key7 
         | Key8 | Key9 | KeyA | KeyB | KeyC | KeyD | KeyE | KeyF
         deriving (Show, Eq, Enum, Ord)

data CPUState = Running | Paused 
    deriving (Show, Eq)

data Machine = Machine
    { _cpu      :: CPU
    , _display  :: Array (Word8, Word8) Word8
    , _key      :: Maybe Key -- current key pressed
    , _drawFlag :: Bool
    } deriving (Show, Eq)

data CPU = CPU 
    { _registerFile :: RegisterFile
    , _stack        :: [Word16]
    , _memory       :: Array Word16 Word8
    , _state        :: CPUState
    , _seed         :: StdGen
    } deriving (Show, Eq)

data RegisterFile = RegisterFile 
    { _regV  :: Array Word8 Word8 -- 16 registers
    , _regI  :: Word16
    , _regDT :: Word8             -- delay timer
    , _regST :: Word8             -- sound timer
    , _regPC :: Word16            -- program counter
    } deriving (Show, Eq)

$(makeLenses ''CPU)
$(makeLenses ''Machine)
$(makeLenses ''RegisterFile)

data Instruction 
    = CLS                   -- 00E0 -- Clear display
    | RET                   -- 00EE -- Return from subroutine
    | SYS Word16            -- 0nnn -- Jump to routine at nnn
    | JP Word16             -- 1nnn -- Jump to location nnn
    | CALL Word16           -- 2nnn -- Call subroutine at nnn
    | SEi Word8 Word8       -- 3xkk -- Skip next instr if Vx = kk
    | SNEi Word8 Word8      -- 4xkk -- Skip next instr if Vx != kk
    | SE Word8 Word8        -- 5xy0 -- Skip next instr if Vx = Vy
    | LDi Word8 Word8       -- 6xkk -- Set Vx = kk
    | ADDi Word8 Word8      -- 7xkk -- Set Vx = Vx + kk
    | LD Word8 Word8        -- 8xy0 -- Set Vx = Vy
    | OR Word8 Word8        -- 8xy1 -- Set Vx = Vx | Vy
    | AND Word8 Word8       -- 8xy2 -- Set Vx = Vx & Vy
    | XOR Word8 Word8       -- 8xy3 -- Set Vx = Vx ^ Vy
    | ADD Word8 Word8       -- 8xy4 -- Set Vx = Vx + Vy, VF = carry
    | SUB Word8 Word8       -- 8xy5 -- Set Vx = Vx - Vy, VF = ~borrow
    | SHR Word8 Word8       -- 8xy6 -- (what is y?) Vx = Vx >> 1. VF = LSB of Vx
    | SUBN Word8 Word8      -- 8xy7 -- Set Vx = Vy - Vx, VF = ~borrow
    | SHL Word8 Word8       -- 8xyE -- Vx = Vx << 1, VF = MSB of Vx
    | SNE Word8 Word8       -- 9xy0 -- Skip next instr if Vx != Vy
    | LDI Word16            -- Annn -- Set I = nnn
    | JPV0 Word16           -- Bnnn -- Jump to location nnn + V0
    | RND Word8 Word8       -- Cxkk -- Set Vx = random byte AND kk
    | DRW Word8 Word8 Word8 -- Dxyn -- Display n-byte sprite starting at memory location I at (Vx, Vy), set Vf = collision
    | SKP Word8             -- Ex9E -- Skip next instruction if key with value of Vx is pressed
    | SKNP Word8            -- ExA1 -- Skip next instruction if key with value of Vx is not pressed
    | LDRD Word8            -- Fx07 -- Set Vx = delay timer value
    | LDRK Word8            -- Fx0A -- Wait for a key press, store the value of the key in Vx
    | LDDR Word8            -- Fx15 -- Set delay timer = Vx
    | LDSR Word8            -- Fx18 -- Set sound timer = Vx
    | ADDIR Word8           -- Fx1E -- Set I = I + Vx
    | LDIS Word8            -- Fx29 -- Set I = location of sprite for digit Vx
    | LDB Word8             -- Fx33 -- Store BCD representation of Vx in memory locations I (hundreds), I+1 (tens), and I+2 (ones)
    | LDIR Word8            -- Fx55 -- Store registers V0 through Vx in memory starting at location I
    | LDRI Word8            -- Fx65 -- Read registers V0 through Vx from memory starting at location I
    | NOOP
    deriving (Eq, Show)


--------------------------------------------------------------------------------
-- data ------------------------------------------------------------------------
--------------------------------------------------------------------------------

displayWidth  = 64 :: Word8
displayHeight = 32 :: Word8

hexSprites :: [Word8]
hexSprites = [ 
    0xF0, 0x90, 0x90, 0x90, 0xF0, -- 0
    0x20, 0x60, 0x20, 0x20, 0x70, -- 1
    0xf0, 0x10, 0xf0, 0x80, 0xf0, -- 2
    0xf0, 0x10, 0xf0, 0x10, 0xf0, -- 3
    0x90, 0x90, 0xf0, 0x10, 0x10, -- 4
    0xf0, 0x80, 0xf0, 0x10, 0xf0, -- 5
    0xf0, 0x80, 0xf0, 0x90, 0xf0, -- 6
    0xf0, 0x10, 0x20, 0x40, 0x40, -- 7
    0xf0, 0x90, 0xf0, 0x90, 0xf0, -- 8
    0xf0, 0x90, 0xf0, 0x10, 0xf0, -- 9
    0xf0, 0x90, 0xf0, 0x90, 0x90, -- A
    0xE0, 0x90, 0xE0, 0x90, 0xE0, -- B
    0xf0, 0x80, 0x80, 0x80, 0xf0, -- C
    0xE0, 0x90, 0x90, 0x90, 0xE0, -- D
    0xf0, 0x80, 0xf0, 0x80, 0xf0, -- E
    0xf0, 0x80, 0xf0, 0x80, 0x80  -- F
    ]

--------------------------------------------------------------------------------
-- initialization --------------------------------------------------------------
--------------------------------------------------------------------------------

newMachine :: (MonadIO io) => io Machine
newMachine = do
    cpu <- newCPU
    return Machine
        { _cpu      = cpu
        , _display  = listArray ((0,0), (displayWidth-1, displayHeight-1)) (repeat 0)
        , _key      = Nothing
        , _drawFlag = False
        }

newCPU :: (MonadIO io) => io CPU
newCPU = do
    gen <- getStdGen
    return CPU
        { _registerFile = RegisterFile 
                         { _regV  = listArray (0, 15) (replicate 16 0)
                         , _regI  = 0
                         , _regDT = 0
                         , _regST = 0
                         , _regPC = 0x200
                         }
        , _stack        = []
        , _memory       = listArray (0, 0x1000 - 1) (repeat 0) // zip [0..] hexSprites
        , _seed         = gen
        , _state        = Running
        }


--------------------------------------------------------------------------------
-- typeclasses -----------------------------------------------------------------
--------------------------------------------------------------------------------

instance Semigroup Word8 where
    a <> b = a + b
instance Monoid Word8 where
    mempty = 0

instance Semigroup Word16 where
    a <> b = a + b
instance Monoid Word16 where
    mempty = 0


--------------------------------------------------------------------------------
-- utility functions -----------------------------------------------------------
--------------------------------------------------------------------------------

getRegV :: (Integral i, Integral j) => CPU -> i -> j
getRegV cpu ind = fromIntegral $ (cpu^.registerFile.regV) ! fromIntegral ind

onRegV :: (Integral i) => CPU -> (Word8 -> Word8) -> i -> Word8
onRegV cpu f i = f (getRegV cpu i)

onRegV2 :: (Integral i) => CPU -> (Word8 -> Word8 -> Word8) -> i -> i -> Word8
onRegV2 cpu f i j = f (getRegV cpu i) (getRegV cpu j)

