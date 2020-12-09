#!/usr/bin/env stack

{-# LANGUAGE OverloadedStrings #-}

import System.Environment
import qualified Data.List as List
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

main :: IO ()
main = do
  args <- getArgs
  output <- getMySeat args
  putStrLn output


getMySeat [fileName] = do
  contents <- TIO.readFile fileName
  let
    seats = T.lines contents
    seatIds = map parseSeatId seats
    sortedIds = List.sort seatIds
    firstId = head sortedIds
    mySeatId = foldl (\expected this -> if expected == this then this + 1 else expected)
                     firstId
                     sortedIds
  return $ show mySeatId


parseSeatId :: T.Text -> Int
parseSeatId seat =
  let
    rowRaw = T.take 7 seat
    row = T.foldl (\acc digit -> 2 * acc + (if digit == 'B' then 1 else 0)) 0 rowRaw
    colRaw = T.takeEnd 3 seat
    col = T.foldl (\acc digit -> 2 * acc + (if digit == 'R' then 1 else 0)) 0 colRaw
  in
    8 * row + col
