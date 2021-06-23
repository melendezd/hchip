{-# LANGUAGE TemplateHaskell #-}
module Types where

import Chip8

import SDL

import Control.Lens
import Control.Lens.TH

data Emulator = Emulator {
    _renderer  :: Renderer,
    _chip8     :: Machine,
    _lastTick  :: Integer,
    _events    :: [Event]
}

$(makeLenses ''Emulator)
