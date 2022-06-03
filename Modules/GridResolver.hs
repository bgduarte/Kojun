module Modules.GridResolver where

import Modules.Grid
import Modules.Cell


type GridAsList = [Cell]
type PossibleCellValues = [Int]
type CoorX = Int
type CoorY = Int
type ResolverCell = (PossibleCellValues, CoorX, CoorY, RegionIndex)
type Region = (RegionIndex, [ResolverCell])
type RegionsList = [Region]
type ResolverCellMessage = (ResolverCell, RegionsList)

-- Transforma a grid em uma lista para facilitar o acesso
getGridAsList :: Grid -> GridAsList
getGridAsList [] = []
getGridAsList (x:xs) = x ++ getGridAsList xs

getRegionIndex :: ResolverCell -> RegionIndex
getRegionIndex (possibleCellValues, coorX, coorY, regionIndex) = regionIndex

regionListToCellList :: RegionsList -> [ResolverCell]
regionListToCellList [] = []
regionListToCellList ((_, x):xs) = x ++ regionListToCellList xs

getRegionByIndex :: RegionIndex -> RegionsList -> Region
getRegionByIndex index regions = head $ filter (\(regionIndex, _) -> regionIndex == index) regions
    
getRegionSize :: Region -> Int
getRegionSize (_, cells) = length cells
    
    --Retorna o indice de uma regiao
getIndex :: Region -> Int
getIndex (index, _) = index

getCellFromCellMessage :: ResolverCellMessage -> ResolverCell
getCellFromCellMessage (cell, _) = cell

-- Descobre o numero de regioes
countRegions :: Grid -> Int
countRegions grid = countRegions' (getGridAsList grid) 0
countRegions' :: GridAsList -> Int -> Int
countRegions' grid count = (maximum regionsIndex) + 1 where regionsIndex = map (\x -> getRegion x) grid

getCellAbove :: ResolverCell -> RegionsList -> ResolverCell
getCellAbove (possibleCellValues, x, y, regionIndex) list = getCellFromCoord (x-1) y list

getCellBelow :: ResolverCell -> RegionsList -> ResolverCell
getCellBelow (possibleCellValues, x, y, regionIndex) list = getCellFromCoord (x+1) y list

getCellLeft :: ResolverCell -> RegionsList -> ResolverCell
getCellLeft (possibleCellValues, x, y, regionIndex) list = getCellFromCoord x (y-1) list

getCellRight :: ResolverCell -> RegionsList -> ResolverCell
getCellRight (possibleCellValues, x, y, regionIndex) list = getCellFromCoord x (y+1) list

getCellFromCoord :: Int -> Int -> RegionsList -> ResolverCell
getCellFromCoord x y regions = 
    if getCellFromCoord' x y regions == [] then
         ([], -1, -1, -1) 
    else 
        (head $ getCellFromCoord' x y regions)
getCellFromCoord' :: Int -> Int -> RegionsList -> [ResolverCell]
getCellFromCoord' x y regions = filter (\(possibleCellValues, x', y', regionIndex) -> x == x' && y == y' ) (regionListToCellList regions)

getAdjacentCells :: ResolverCellMessage -> [ResolverCell]
getAdjacentCells (cell, regions) = [getCellRight cell regions] ++ [getCellLeft cell regions] ++ [getCellAbove cell regions] ++ [getCellBelow cell regions] 

getAdjacentCellValues :: ResolverCellMessage -> [Int]
getAdjacentCellValues (cell, regions) = adjValues  
    where
        adjValues = map (\x -> head x) adjValidValues
        adjValidValues = filter (\x -> length x == 1) adjValuesList
        adjValuesList = map (\(values, _, _, _) ->  values) (getAdjacentCells (cell, regions))


--Cria a lista de regioes, porem vazia
createEmptyRegionsList :: Int -> RegionsList
createEmptyRegionsList count = createEmptyRegionsList' count []
createEmptyRegionsList' :: Int -> RegionsList -> RegionsList
createEmptyRegionsList' 0 list = list
createEmptyRegionsList' count list = createEmptyRegionsList' (count - 1) ((count-1, []):list)

--Cria uma resolverCell baseada numa cell comum
makeResolverCell :: Int -> Int -> Cell -> ResolverCell
makeResolverCell x y cell = ((getPossibleCellValues cell), x, y, getRegion cell)


-- Adiciona uma celula para regiao que que tem o indice que foi passado
--indice da regiao --> Celula -> Lista de regioes -> lista de regioes
addToRegion :: Int -> ResolverCell -> RegionsList -> RegionsList
addToRegion index cell list = map (\x -> addCell x cell index) list

-- Adiciona uma celula em uma regia se o indice passado é igual ao indice da regiao
addCell :: Region -> ResolverCell -> Int -> Region 
addCell (index, cells) cell i = 
    if i == index then 
        (index, cells ++ [cell])
    else 
        (index, cells)

-- Tranforma a grid em uma lista de regioes
toRegionsList :: Grid -> RegionsList
toRegionsList grid = initResolveRegions (toRegionsList' grid 0 0 ((getLenghtGrid grid)-1) (createEmptyRegionsList (countRegions grid)))
-- Grid -> x -> y -> lenght -> RegionsList
toRegionsList' :: Grid -> Int -> Int -> Int -> RegionsList -> RegionsList
toRegionsList' (h:tail) x y len regions = 
    if  x < len then 
        toRegionsList' tail (x+1) y len (toRegionListRow h x 0 len regions)
    else 
        toRegionListRow h x 0 len regions

--Funcao auxiliar a toRegionsList que tranforma uma linha
toRegionListRow :: Row -> Int -> Int -> Int -> RegionsList -> RegionsList
toRegionListRow (h:tail) x y len regions = 
    if  y < len then 
        toRegionListRow tail x (y+1) len ((addToRegion (getRegion h) (makeResolverCell x y h) regions) )
    else 
        addToRegion (getRegion h) (makeResolverCell x y h) regions

--Pega os valores em uma regiao que ja foram decididos
getDecidedValues :: Region -> [Int]
getDecidedValues (index, cells) = map (\x -> head x) decidedValues 
    where decidedValues = filter (\x -> (length x == 1)) possibleValues
          possibleValues = map (\x -> getPossibleValues x) cells

getPossibleValues :: ResolverCell -> [Int]
getPossibleValues (possibleValues, _, _, _) = possibleValues

--Preenhe a celula com todos os valores possiveis caso ela ja nao tem um valor definido
getPossibleCellValues :: Cell -> PossibleCellValues
getPossibleCellValues cell = 
    if getValue cell == 0 then 
        [1..9]
    else 
        [getValue cell]

--Funcao para printar a lista de regioes (DEBUG)
getPrintRegions :: RegionsList -> String
getPrintRegions [] = ""
getPrintRegions (h:tail) = getPrintRegion h ++ "\n"++ getPrintRegions tail

--Funcao auxilar de getPrintRegions que printa uma unica regiao
getPrintRegion :: Region -> String
getPrintRegion (index, cells) = ("Regiao " ++ show index ++ ":" ++ show cells)


initResolveRegions :: RegionsList -> RegionsList
initResolveRegions regions = (map (\x -> initResolveRegion x regions) regions)

initResolveRegion :: Region -> RegionsList -> Region
initResolveRegion (regionIndex, cells) regions = (regionIndex, (map (\x -> (initResolveCell x regions))) cells)

initResolveCell :: ResolverCell -> RegionsList -> ResolverCell
initResolveCell (possibleCellValues, x, y, regionIndex) list = 
    if length possibleCellValues == 1 then 
        (possibleCellValues, x, y, regionIndex)
    else 
        getCellFromCellMessage(initResolveCell' ((possibleCellValues, x, y, regionIndex), list))

initResolveCell' :: ResolverCellMessage -> ResolverCellMessage
initResolveCell' = excludeByRegionSize

fromRegionsListToGrid :: RegionsList -> Int -> Grid  
fromRegionsListToGrid list gridSize = fromRegionsListToGrid' list gridSize 0 0

fromRegionsListToGrid' :: RegionsList -> Int -> Int -> Int -> Grid
fromRegionsListToGrid' list size x y =
    if x < size then 
        (fromRegionsListToGridRow list size x 0) : (fromRegionsListToGrid' list size (x+1) y)
    else
        []

fromRegionsListToGridRow  :: RegionsList -> Int -> Int -> Int -> Row
fromRegionsListToGridRow list size x y = 
    if y < size then 
        getTrueCell(getCellFromCoord x y list) : (fromRegionsListToGridRow list size x (y+1))
    else
        []

--Converte uma celula de resolucao para uma celula da grid
getTrueCell :: ResolverCell -> Cell
getTrueCell (values, _, _, regionIndex) = (regionIndex, value) 
    where value = if (length values) == 1 then head values else 0






-- ============================================================================================================================
-- Exclude Functions
-- ============================================================================================================================
excludeByRegionSize :: ResolverCellMessage -> ResolverCellMessage
excludeByRegionSize ((possibleCellValues, x, y, regionIndex), list) = updateRegionsList ((newPossibleValues, x, y, regionIndex), list )
    where newPossibleValues = [1..(getRegionSize(getRegionByIndex regionIndex list))]

excludeForOtherCellsInRegion :: ResolverCellMessage -> ResolverCellMessage
excludeForOtherCellsInRegion ((possibleCellValues, x, y, regionIndex), list) = updateRegionsList ((cellValues , x, y, regionIndex), list) 
    where cellValues = (filter (`notElem` getDecidedValues ((getRegionByIndex regionIndex) list))) possibleCellValues

excludeForAdjacentCells :: ResolverCellMessage -> ResolverCellMessage
excludeForAdjacentCells ((possibleCellValues, x, y, regionIndex) , list) = updateRegionsList ((cellValues , x, y, regionIndex), list) where
    cellValues = (filter (`notElem` (getAdjacentCellValues ((possibleCellValues, x, y, regionIndex), list))) possibleCellValues)

excludeForBelowInSameRegion :: ResolverCellMessage -> ResolverCellMessage
excludeForBelowInSameRegion ((possibleCellValues, x, y, regionIndex), list) = updateRegionsList ((cellValues , x, y, regionIndex), list) where
    cellValues = 
        if length belowCellValues > 0 && (getRegionIndex cellBelow == regionIndex) then 
            filter (> (minimum belowCellValues)) possibleCellValues
        else
            possibleCellValues
    belowCellValues = ( getPossibleValues cellBelow)
    cellBelow = getCellBelow (possibleCellValues, x, y, regionIndex) list

excludeForAboveInSameRegion :: ResolverCellMessage -> ResolverCellMessage
excludeForAboveInSameRegion ((possibleCellValues, x, y, regionIndex), list) = updateRegionsList ((cellValues , x, y, regionIndex), list) where
    cellValues = 
        if length aboveCellValues > 0 && (getRegionIndex cellAbove == regionIndex) then 
            filter (< (maximum aboveCellValues)) possibleCellValues
        else
            possibleCellValues
    aboveCellValues = ( getPossibleValues cellAbove)
    cellAbove = getCellAbove (possibleCellValues, x, y, regionIndex) list

checkForOnlyCellPossibleForAValue :: ResolverCellMessage -> ResolverCellMessage
checkForOnlyCellPossibleForAValue ((possibleCellValues, x, y, regionIndex), list) = updateRegionsList ((cellValues , x, y, regionIndex), list) where
    cellValues = 
        if length cellValuesThatOnlyThisCellHas == 1 then 
            cellValuesThatOnlyThisCellHas 
        else
            possibleCellValues
    cellValuesThatOnlyThisCellHas = filter (`notElem` possibleValuesFromOtherCellsInRegion) possibleCellValues
    possibleValuesFromOtherCellsInRegion = concat $ map (getPossibleValues) otherCellsInRegion
    otherCellsInRegion = filter (/=(possibleCellValues, x, y, regionIndex)) allCellsInRegion
    allCellsInRegion = head (map (\(_, cells) -> cells) [(getRegionByIndex regionIndex list)])

check ((possibleCellValues, x, y, regionIndex), list) = cellValuesThatOnlyThisCellHas where
    cellValuesThatOnlyThisCellHas = filter (`notElem` possibleValuesFromOtherCellsInRegion) possibleCellValues
    possibleValuesFromOtherCellsInRegion = concat $ map (getPossibleValues) otherCellsInRegion
    otherCellsInRegion = filter (/=(possibleCellValues, x, y, regionIndex)) allCellsInRegion
    allCellsInRegion = head (map (\(_, cells) -> cells) [(getRegionByIndex regionIndex list)])



updateRegionsList :: ResolverCellMessage -> ResolverCellMessage
updateRegionsList (cell , list) = (cell , updateRegionsList' (cell, list) )

updateRegionsList' :: ResolverCellMessage -> RegionsList
updateRegionsList' (cell , []) = [] 
updateRegionsList' ((values, x, y, regionIndex) , (h:tail)) = 
    if (getIndex h) == regionIndex then 
       [(updateRegion h (values, x, y, regionIndex))] ++ (updateRegionsList' ((values, x, y, regionIndex) , tail))
    else 
        [(h)] ++ (updateRegionsList' ((values, x, y, regionIndex) , tail))


updateRegion :: Region -> ResolverCell -> Region
updateRegion (index, cells) (values, x, y, regionIndex) = (index, (updateCell (values, x, y, regionIndex) cells))

updateCell :: ResolverCell -> [ResolverCell] -> [ResolverCell]
updateCell (values, x, y, regionIndex) [] = []
updateCell (values, x, y, regionIndex) (h:tail) =
    if hasTheSameCoord h (values, x, y, regionIndex) then
        (values, x, y, regionIndex) : (updateCell (values, x, y, regionIndex) tail)
    else
        h : (updateCell (values, x, y, regionIndex) tail)

hasTheSameCoord :: ResolverCell -> ResolverCell -> Bool
hasTheSameCoord (values, x, y, regionIndex) (values2, x2, y2, regionIndex2) = (x == x2) && (y == y2)








resolve :: Grid -> Grid
resolve grid = fromRegionsListToGrid(resolveFromRegionList ((toRegionsList grid))) 6 --TODO: figure how to get this

resolveFromRegionList :: RegionsList -> RegionsList
resolveFromRegionList list = resolveFromRegionsList' (list) list

resolveFromRegionsList' :: RegionsList -> RegionsList -> RegionsList
resolveFromRegionsList' [] _ = [] 
resolveFromRegionsList' (h:tail) holeList = ((resolveRegion h holeList) : (resolveFromRegionsList' tail holeList))


notResolved :: RegionsList -> Bool -- arrumar o nome
notResolved list = length cellsNonResolved == 0 where
    cellsNonResolved = (filter( \x -> (length x > 1)) allCellsPossibleValues)
    allCellsPossibleValues = (map (\x -> getPossibleValues x) allCells)
    allCells = (regionListToCellList list)


resolveRegion :: Region -> RegionsList -> Region
resolveRegion (regionIndex, cells) list = (regionIndex, resolveCells cells list)

resolveCells :: [ResolverCell] -> RegionsList ->[ResolverCell]
resolveCells [] list = []
resolveCells (h:tail) list = (resolveCell h list) : (resolveCells tail list)

resolveCell :: ResolverCell -> RegionsList -> ResolverCell
resolveCell (possibleCellValues, x, y, regionIndex) list = 
    if length possibleCellValues == 1 then 
        (possibleCellValues, x, y, regionIndex)
    else 
        getCellFromCellMessage(resolveCell' ((possibleCellValues, x, y, regionIndex),  list))

getListFromResolverMessage :: ResolverCellMessage -> RegionsList
getListFromResolverMessage (_, list) = list

resolveCell' :: ResolverCellMessage -> ResolverCellMessage
resolveCell' message =   checkForOnlyCellPossibleForAValue(excludeForAboveInSameRegion(excludeForBelowInSameRegion (excludeForAdjacentCells (excludeForOtherCellsInRegion message) ))) -- . excludeForBelowInSameRegion . excludeForAboveInSameRegion
