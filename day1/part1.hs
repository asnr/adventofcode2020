-- To run: $ stack part1.hs FILE_NAME

import System.Environment

main :: IO()
main = getArgs >>= reportRepair >>= putStrLn

targetSum = 2020

reportRepair [fileName] = do
  expenseReport <- readFile fileName
  let lineItems = lines expenseReport
      (fstSummand, sndSummand) = findSummands $ map read lineItems
  return $ show (fstSummand * sndSummand)
reportRepair _ = return "Usage: $ part1 FILE_NAME"

findSummands :: [Int] -> (Int, Int)
findSummands lineItems = findSummandsAcc [] lineItems

findSummandsAcc :: [Int] -> [Int] -> (Int, Int)
findSummandsAcc targetSummands (item : lineItems) =
  if any (\target -> target == item) targetSummands
  then (targetSum - item, item)
  else findSummandsAcc ((targetSum - item) : targetSummands) lineItems
