import System.Environment

main :: IO()
main = do
  args <- getArgs
  output <- reportRepair args
  putStrLn output

targetSum = 2020

reportRepair [fileName] = do
  expenseReport <- readFile fileName
  let lineItems = lines expenseReport
      (fstSummand, sndSummand, thdSummand) = findSummandsBrute $ map read lineItems
  return $ show (fstSummand * sndSummand * thdSummand)
reportRepair _ = return "Usage: part2 FILE_NAME"

findSummandsBrute :: [Int] -> (Int, Int, Int)
findSummandsBrute lineItems = head [ (x, y, z) | x <- lineItems,
                                                 y <- lineItems,
                                                 z <- lineItems,
                                                 x + y + z == targetSum ]
