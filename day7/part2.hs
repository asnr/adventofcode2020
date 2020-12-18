#!/usr/bin/env stack

{-# LANGUAGE OverloadedStrings#-}

import qualified Data.Text as T
import qualified Data.Map as Map
import qualified Data.Set as Set

import Input

main :: IO ()
main = putStrLn $ show $ numChildBags "shiny gold" puzzle

numChildBags parent rawRules =
  let
    rules = T.lines rawRules
    parentToChildren = parseRules rules
  in
    sumChildBags parent parentToChildren

parseRules :: [T.Text] -> Map.Map T.Text [(T.Text, Int)]
parseRules [] = Map.empty
parseRules (rule:rules) =
  let
    [parent, childrenRaw] = T.splitOn " bags contain " rule
    hasNoChildren = childrenRaw == "no other bags."
    childrenRawList = T.splitOn ", " childrenRaw
    children = if hasNoChildren then [] else map extractChild childrenRawList
  in
    Map.insert parent children $ parseRules rules

extractChild :: T.Text -> (T.Text, Int)
extractChild rawChild =
  let
    (quantityText:quality:colour:_) = T.words rawChild
    quantity = read (T.unpack quantityText) :: Int
  in
    (T.intercalate " " [quality, colour], quantity)


sumChildBags :: T.Text -> Map.Map T.Text [(T.Text, Int)] -> Int
sumChildBags parent parentToChildren =
  let
    children = Map.findWithDefault [] parent parentToChildren
  in
    sum $
      map (\(child, quantity) -> quantity * (1 + sumChildBags child parentToChildren))
          children

