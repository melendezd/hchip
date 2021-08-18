{-# LANGUAGE OverloadedStrings #-}

module Main where

import Chip8
import Control.Lens
import Control.Lens.Operators
import Control.Monad
import Data.Array
import qualified Data.ByteString as BS
import Data.Map (Map)
import qualified Data.Map as M
import Data.Time.Clock.POSIX (getPOSIXTime)
import Linear (V4 (..))
import Numeric (showHex)
import SDL
import System.Environment
import Types

{-
testProg = listArray (0,8) [ LDi 1 0xF
                       , LDi 0 0   -- reset
                       , LDIS 0    -- load_sprite
                       , CLS
                       , DRW 5 2 2 -- Draw V0 value digit
                       , ADDi 0 1  -- Increment V0
                       , SE 0 1    -- If V0 != 0xF...
                       , JP 2      -- Go to load_sprite
                       , JP 1      -- Else, go to reset
                       ]
                       -}

keyMap :: Map Scancode Key
keyMap =
  M.fromList $
    zip
      [Scancode1, Scancode2, Scancode3, Scancode4, ScancodeQ, ScancodeW, ScancodeE, ScancodeR, ScancodeA, ScancodeS, ScancodeD, ScancodeF, ScancodeZ, ScancodeX, ScancodeC, ScancodeV]
      [Key1, Key2, Key3, KeyC, Key4, Key5, Key6, KeyD, Key7, Key8, Key9, KeyE, KeyA, Key0, KeyB, KeyF]

nanoTime :: IO Integer

t mul = round . (mul *) <$> getPOSIXTime

nanoTime = t 1000000000

main :: IO ()
main = do
  initializeAll
  window <- createWindow "fuck" (defaultWindow {windowInitialSize = V2 (32 * 20) (16 * 20)})
  renderer <- createRenderer window (-1) defaultRenderer
  rendererScale renderer $= V2 10 10
  machine <- newMachine

  fname : _ <- getArgs
  rom <- BS.readFile fname
  --print $ BS.index rom 0
  --print $ BS.index rom 1
  --print $ BS.index rom 2
  --print $ BS.index rom 3
  --print $ BS.index rom 4
  --print $ BS.index rom 5
  startTime <- nanoTime
  loop $
    Emulator
      { _renderer = renderer,
        _chip8 = loadROM rom machine,
        _lastTick = startTime,
        _events = []
      }

isKeyboardEvent event = case eventPayload event of
  KeyboardEvent _ -> True
  _ -> False

isPressEvent :: Event -> Bool
isPressEvent e = case eventPayload e of
  KeyboardEvent ke -> keyboardEventKeyMotion ke == Pressed
  _ -> False

isReleaseEvent :: Event -> Bool
isReleaseEvent e = case eventPayload e of
  KeyboardEvent ke -> keyboardEventKeyMotion ke == Released
  _ -> False

getKey :: Event -> Maybe Key
getKey e = case eventPayload e of
  KeyboardEvent ke -> M.lookup (keysymScancode $ keyboardEventKeysym ke) keyMap

getReleasedKeys :: [Event] -> [Maybe Key]
getReleasedKeys = fmap getKey . filter isReleaseEvent

getPressedKey :: [Event] -> Maybe Key
getPressedKey events = case dropWhile (not . isPressEvent) events of
  [] -> Nothing
  e : _ -> case eventPayload e of
    KeyboardEvent ke -> M.lookup (keysymScancode $ keyboardEventKeysym ke) keyMap
    _ -> Nothing

herz = 500

timeStep = 1000000000 `div` herz

loop :: Emulator -> IO ()
loop emu = do
  events <- pollEvents
  let pressedKey = getPressedKey events
  let releasedKeys = getReleasedKeys events
  let emu' = case pressedKey of
        Nothing ->
          if emu ^. chip8 . key `elem` releasedKeys
            then emu & chip8 . key .~ Nothing
            else emu
        Just _ -> emu & (chip8 . key) .~ pressedKey
  let lastTime = emu' ^. lastTick
      rend = emu' ^. renderer
      machine = emu' ^. chip8
  time <- nanoTime
  if time - lastTime < timeStep
    then loop emu'
    else do
      when (machine ^. drawFlag) $ do
        clear rend
        render rend machine
        present rend
      let eMachine' = updateMachine machine
      now <- nanoTime
      case eMachine' of
        Right machine' -> loop emu''
          where
            emu'' =
              emu' & chip8 .~ machine'
                & lastTick .~ now
                & over (chip8 . cpu . registerFile . regDT) (\x -> if x > 0 then x -1 else 0)
                & over (chip8 . cpu . registerFile . regST) (\x -> if x > 0 then x -1 else 0)
        Left err -> print (currentOp machine) >> print err

render :: Renderer -> Machine -> IO ()
render renderer m =
  forM_
    (assocs (m ^. display))
    ( \((x, y), pix) -> do
        rendererDrawColor renderer $= V4 0 0 0 255
        if pix == 0
          then rendererDrawColor renderer $= V4 0 0 0 255
          else rendererDrawColor renderer $= V4 255 0 0 255
        drawPoint renderer (P $ V2 (fromIntegral x) (fromIntegral y))
    )
