#!/usr/bin/env stack

{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import qualified Data.Array as Array
import qualified Data.Set as Set

import Input

data Instr = Acc Int | Jmp Int | Nop

main = print $ valueAtInfiniteLoop input

valueAtInfiniteLoop code =
  let
    codeLines = T.lines code
    parsedLines = map parseInstruction codeLines
    instructions =   Array.listArray (0, length codeLines - 1) parsedLines
  in
    runInstructions instructions 0 Set.empty 0

parseInstruction rawLine
  | name == "acc" = Acc (read $ T.unpack value :: Int)
  | name == "jmp" = Jmp (read $ T.unpack value :: Int)
  | name == "nop" = Nop
  where name = T.take 3 rawLine
        -- read cannot parse int string starting with '+', e.g. "+3"
        value = T.drop (if T.index rawLine 4 == '+' then 5 else 4) rawLine

runInstructions instructions pos pastPoss acc =
  if Set.member pos pastPoss
  then acc
  else case instructions Array.! pos of
    Acc val -> runInstructions instructions (pos + 1) (Set.insert pos pastPoss) (acc + val)
    Jmp val -> runInstructions instructions (pos + val) (Set.insert pos pastPoss) acc
    Nop -> runInstructions instructions (pos + 1) (Set.insert pos pastPoss) acc
