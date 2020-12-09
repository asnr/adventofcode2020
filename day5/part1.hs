#!/usr/bin/env stack

{-# LANGUAGE OverloadedStrings #-}

import System.Environment
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

main :: IO ()
main = do
  args <- getArgs
  output <- maxSeatId args
  putStrLn output

maxSeatId :: [String] -> IO String
maxSeatId [fileName] = do
  contents <- TIO.readFile fileName
  let
    seats = T.lines contents
    seatIds = map parseSeatId seats
  return $ show $ maximum seatIds
maxSeatId _ = return "Usage: part1.hs FILE_NAME"


parseSeatId :: T.Text -> Int
parseSeatId seat =
  let
    rowRaw = T.take 7 seat
    row = T.foldl (\acc digit -> 2 * acc + (if digit == 'B' then 1 else 0)) 0 rowRaw
    colRaw = T.takeEnd 3 seat
    col = T.foldl (\acc digit -> 2 * acc + (if digit == 'R' then 1 else 0)) 0 colRaw
  in
    8 * row + col
