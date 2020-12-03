-- $ cabal install --lib regex-posix
-- Compile library using
-- $ stack ghci --package regex-posix
-- Then run program with
-- $ stack part1.hs INPUT_FILE

import System.Environment
import Text.Regex.Posix

main :: IO()
main = do
  args <- getArgs
  output <- numValidPasswordsInFile args
  putStrLn output

numValidPasswordsInFile :: [String] -> IO String
numValidPasswordsInFile [fileName] = do
  contents <- readFile fileName
  let passwordLines = lines contents
  return $ show $ numValidPasswords passwordLines
numValidPasswordsInFile _ = return "Usage: part1 FILE_NAME"

numValidPasswords :: [String] -> Int
numValidPasswords passwordLines = length $ filter isValidPassword passwordLines

isValidPassword :: String -> Bool
isValidPassword passwordLine =
  case passwordLine =~ "([0-9]+)-([0-9]+) ([a-z]): (.+)":: (String,String,String,[String]) of
    (_, _, _, [minFreq, maxFreq, letter, password]) ->
      letterFreqInRange (head letter) (read minFreq :: Int) (read maxFreq :: Int) password
    _ -> False

letterFreqInRange :: Char -> Int -> Int -> String -> Bool
letterFreqInRange letter minFreq maxFreq password =
  let letterFreq = length $ filter (== letter) password in
    minFreq <= letterFreq && letterFreq <= maxFreq
