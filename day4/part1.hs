-- To run: $ stack part1.hs FILE_NAME

{-# LANGUAGE OverloadedStrings #-}

import System.Environment
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Set as Set

requiredPassportKeys :: Set.Set T.Text
requiredPassportKeys = Set.fromList ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

main :: IO ()
main = do
  args <- getArgs
  output <- passportValidator args
  print output

passportValidator :: [String] -> IO String
passportValidator [fileName] = do
  contents <- TIO.readFile fileName
  return $ show $ length $ filter isValidPassport $ T.splitOn "\n\n" contents
passportValidator _ = return "Usage: part1.hs FILE_NAME"

isValidPassport :: T.Text -> Bool
isValidPassport passport =
  let
    fields = T.words passport
    fieldKeys = map (head . T.splitOn ":") fields
    fieldKeySet = Set.fromList fieldKeys
  in
    requiredPassportKeys `Set.isSubsetOf` fieldKeySet
