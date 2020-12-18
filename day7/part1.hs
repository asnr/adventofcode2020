#!/usr/bin/env stack

{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import qualified Data.Map as Map
import qualified Data.Set as Set

import Input

main :: IO ()
main = putStrLn $ show $ numContainerColours "shiny gold" puzzle

numContainerColours child rawRules =
  let
    rules = T.lines rawRules
    childToParents = parseRules rules
    parents = Map.findWithDefault Set.empty child childToParents
  in
    Set.size $ breadthFirstSearch childToParents parents

parseRules :: [T.Text] -> Map.Map T.Text (Set.Set T.Text)
parseRules [] = Map.empty
parseRules (rule:rules) =
  let
    [parent, childrenRaw] = T.splitOn " bags contain " rule
    hasNoChildren = childrenRaw == "no other bags."
    childrenRawList = T.splitOn ", " childrenRaw
    children = if hasNoChildren then [] else map extractChild childrenRawList
    childParentList = map (\child -> (child, Set.singleton parent)) children
    thisRule = Map.fromList childParentList
  in
    Map.unionWith Set.union thisRule $ parseRules rules

extractChild :: T.Text -> T.Text
extractChild rawChild = T.intercalate " " $ take 2 $ tail $ T.words rawChild

breadthFirstSearch childToParents ancestors =
  let
    parentsOfAncestors = foldl (\parents ancestor -> Set.union parents $ Map.findWithDefault Set.empty ancestor childToParents) Set.empty ancestors
    nextAncestors = Set.union ancestors parentsOfAncestors
    ancestorsUnchanged = Set.size ancestors == Set.size nextAncestors
  in
    if ancestorsUnchanged then nextAncestors else breadthFirstSearch childToParents nextAncestors
