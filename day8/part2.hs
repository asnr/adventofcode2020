#!/usr/bin/env stack

{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import qualified Data.Array as Array
import qualified Data.Set as Set

import Input

data Instr = Acc Int | Jmp Int | Nop Int

main = print $ fixAndRun input

fixAndRun code =
  let
    codeLines = T.lines code
    instructionsList = map parseInstruction codeLines
    instructions = Array.listArray (0, length instructionsList - 1) instructionsList
    initAcc = 0
    initIdx = 0
  in
    exploreAndRunFixes initAcc Set.empty instructions initIdx

parseInstruction rawLine
  | name == "acc" = Acc (read $ T.unpack value :: Int)
  | name == "jmp" = Jmp (read $ T.unpack value :: Int)
  | name == "nop" = Nop (read $ T.unpack value :: Int)
  where name = T.take 3 rawLine
        -- read cannot parse int string starting with '+', e.g. "+3"
        value = T.drop (if T.index rawLine 4 == '+' then 5 else 4) rawLine

exploreAndRunFixes acc seenIdxs instructions idx =
  let
    nextSeenIdxs = Set.insert idx seenIdxs
  in
    case instructions Array.! idx of
      Acc val -> exploreAndRunFixes (acc + val) nextSeenIdxs instructions (idx + 1)
      Jmp val ->
        case runFix acc nextSeenIdxs instructions (idx + 1) of
          Left idxs -> exploreAndRunFixes acc idxs instructions (idx + val)
          Right finalAcc -> finalAcc
      Nop val ->
        case runFix acc nextSeenIdxs instructions (idx + val) of
          Left idxs -> exploreAndRunFixes acc idxs instructions (idx + 1)
          Right finalAcc -> finalAcc

runFix :: Int -> Set.Set Int -> Array.Array Int Instr -> Int -> Either (Set.Set Int) Int
runFix acc seenIdxs instructions idx
  | Set.member idx seenIdxs               = Left seenIdxs
  | idx > snd (Array.bounds instructions) = Right acc
  | otherwise                             =
      let
        nextSeenIdxs = Set.insert idx seenIdxs
      in
        case instructions Array.! idx of
          Acc val -> runFix (acc + val) nextSeenIdxs instructions (idx + 1)
          Jmp val -> runFix acc nextSeenIdxs instructions (idx + val)
          Nop _   -> runFix acc nextSeenIdxs instructions (idx + 1)
