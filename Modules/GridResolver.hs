{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use map" #-}
module Modules.GridResolver where

import Modules.Grid
import Modules.Cell

type GridAsList = [Cell]
type PossibleCellValues = [Int]
type ResolverCell = (RegionIndex, PossibleCellValues)
type ResolverGrid = [ResolverCell]
type Region = [ResolverCell]
type RegionsList = [Region]

getRegionIndex :: ResolverCell -> RegionIndex
getRegionIndex (regionIndex, _) = regionIndex 

getPossibleValues :: Cell -> ResolverCell
getPossibleValues (region, value) = if value == 0 then (region, [1..9]) else (region, [value])

toResolverGrid :: Grid -> ResolverGrid
toResolverGrid grid = map (\(regionIndex, value) -> getPossibleValues (regionIndex, value)) getGridAsList grid

fromResolverGrid :: ResolverGrid -> Grid
fromResolverGrid resolverGrid = map (\(regionIndex, value) -> (regionIndex, value)) resolverGrid

getGridAsList :: Grid -> GridAsList
getGridAsList [] = []
getGridAsList (x:xs) = x ++ getGridAsList xs