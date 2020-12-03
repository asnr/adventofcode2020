-- $ cabal install --lib regex-posix
-- Compile library using
-- $ stack ghci --package regex-posix
-- Then run program with
-- $ stack part2.hs INPUT_FILE

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
numValidPasswordsInFile _ = return "Usage: part2 FILE_NAME"

numValidPasswords :: [String] -> Int
numValidPasswords passwordLines = length $ filter isValidPassword passwordLines

isValidPassword :: String -> Bool
isValidPassword passwordLine =
  case passwordLine =~ "([0-9]+)-([0-9]+) ([a-z]): (.+)":: (String,String,String,[String]) of
    (_, _, _, [fstIdx, sndIdx, letter, password]) ->
      letterAtExactlyOneIndex (head letter) (read fstIdx :: Int) (read sndIdx :: Int) password
    _ -> False


letterAtExactlyOneIndex :: Char -> Int -> Int -> String -> Bool
letterAtExactlyOneIndex letter fstIdx sndIdx password =
  let fstChar = password !! (fstIdx - 1)
      sndChar = password !! (sndIdx - 1) in
  (fstChar == letter && sndChar /= letter) || (fstChar /= letter && sndChar == letter)
