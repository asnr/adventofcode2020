-- To run: $ stack part2.hs FILE_NAME

import System.Environment

main :: IO ()
main = do
  args <- getArgs
  output <- productTreesHitMain args
  putStrLn output

productTreesHitMain :: [String] -> IO String
productTreesHitMain [fileName] = do
  rawMapTile <- readFile fileName
  return $ show $ productTreesHit rawMapTile
productTreesHitMain _ = return "Usage: part2 FILE_NAME"

slopes :: [(Int, Int)]
slopes = [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]

productTreesHit :: String -> Int
productTreesHit rawMapTile =
  let mapLines = lines rawMapTile
      treesHitForSlopes = map (treesHitThisLineAndBelow mapLines 0) slopes
  in
    product treesHitForSlopes

treesHitThisLineAndBelow :: [String] -> Int -> (Int, Int) -> Int
treesHitThisLineAndBelow [] _ _ = 0
treesHitThisLineAndBelow (mapLine : mapLines) lineNum (slopeX, slopeY) =
  let position = ((lineNum `quot` slopeY) * slopeX) `mod` length mapLine
      hitsThisLine =
        if lineNum `mod` slopeY == 0 && mapLine !! position == '#' then 1 else 0
  in hitsThisLine + treesHitThisLineAndBelow mapLines (lineNum + 1) (slopeX, slopeY)
