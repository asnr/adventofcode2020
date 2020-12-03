-- To run: $ stack part1.hs FILE_NAME

import System.Environment

main :: IO ()
main = do
  args <- getArgs
  output <- numTreesHitMain args
  putStrLn output

numTreesHitMain :: [String] -> IO String
numTreesHitMain [fileName] = do
  rawMapTile <- readFile fileName
  return $ show $ numTreesHit rawMapTile
numTreesHitMain _ = return "Usage: part1 FILE_NAME"

numTreesHit :: String -> Int
numTreesHit rawMapTile =
  let mapLines = lines rawMapTile in
    treesHitThisLineAndBelow mapLines 0

treesHitThisLineAndBelow :: [String] -> Int -> Int
treesHitThisLineAndBelow [] _ = 0
treesHitThisLineAndBelow (mapLine : mapLines) lineNum =
  let position = (lineNum * 3) `mod` length mapLine
      hitsThisLine = if mapLine !! position == '#' then 1 else 0
  in hitsThisLine + treesHitThisLineAndBelow mapLines (lineNum + 1)
