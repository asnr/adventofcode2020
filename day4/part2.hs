#!/usr/bin/env stack
-- stack --resolver lts-16.24 script

{-# LANGUAGE OverloadedStrings #-}

import System.Environment
import Text.Regex.TDFA
import Text.Read
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Set as Set

requiredPassportKeys :: [(T.Text, T.Text -> Bool)]
requiredPassportKeys = [
  ("byr", (\x -> (x =~ T.pack "[0-9]{4}" :: Bool) && isIntBetween 1920 2002 x)),
  ("iyr", (\x -> (x =~ T.pack "[0-9]{4}" :: Bool) && isIntBetween 2010 2020 x)),
  ("eyr", (\x -> (x =~ T.pack "[0-9]{4}" :: Bool) && isIntBetween 2020 2030 x)),
  ("hgt", (\x ->
             (T.takeEnd 2 x == "cm" && isIntBetween 150 193 (T.dropEnd 2 x)) ||
             (T.takeEnd 2 x == "in" && isIntBetween 59 76 (T.dropEnd 2 x)))),
  ("hcl", (\x -> x =~ T.pack "^#[0-9a-f]{6}$" :: Bool)),
  ("ecl", (\x -> x =~ T.pack "^amb|blu|brn|gry|grn|hzl|oth$" :: Bool)),
  ("pid", (\x -> x =~ T.pack "^[0-9]{9}$" :: Bool))]

main :: IO ()
main = do
  args <- getArgs
  output <- passportValidator args
  print output


isIntBetween lowerBd upperBd t = case readMaybe $ T.unpack t :: Maybe Int of
  Just x -> lowerBd <= x && x <= upperBd
  Nothing -> False

passportValidator :: [String] -> IO T.Text
passportValidator [fileName] = do
  contents <- TIO.readFile fileName
  return $ T.pack $ show $ length $ filter isValidPassport $ T.splitOn "\n\n" contents
passportValidator _ = return "Usage: part1.hs FILE_NAME"

isValidPassport :: T.Text -> Bool
isValidPassport passport =
  let
    fields = T.words passport
    keyValues = map splitKeyValue fields
    fieldMap = Map.fromList keyValues
  in
    all (hasKey fieldMap) requiredPassportKeys


splitKeyValue :: T.Text -> (T.Text, T.Text)
splitKeyValue raw =
  let
    (_, _, _, submatches) = raw =~ T.pack "([^:]*):(.*)" :: (T.Text, T.Text, T.Text, [T.Text])
  in
    (submatches !! 0, submatches !! 1)

hasKey :: Map.Map T.Text T.Text -> (T.Text, T.Text -> Bool) -> Bool
hasKey fieldMap (key, isValid) =
  (Map.member key fieldMap) && isValid (fieldMap Map.! key)
