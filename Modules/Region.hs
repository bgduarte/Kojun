module Modules.Region where
import Modules.Grid
import Modules.Cell

type GridAsList = [Cell]
type PossibleCellValues = [Int]
type CoorX = Int
type CoorY = Int
type ResolverCell = (PossibleCellValues, CoorX, CoorY, RegionIndex)
type Region = (RegionIndex, [ResolverCell])
type RegionsList = [Region]


-- Transforma a grid em uma lista para facilitar o acesso
getGridAsList :: Grid -> GridAsList
getGridAsList [] = []
getGridAsList (x:xs) = x ++ getGridAsList xs

getRegionByIndex :: RegionIndex -> RegionsList -> Region
getRegionByIndex index regions = head $ filter (\(regionIndex, _) -> regionIndex == index) regions
    
getRegionSize :: Region -> Int
getRegionSize (_, cells) = length cells
    
    --Retorna o indice de uma regiao
getIndex :: Region -> Int
getIndex (index, _) = index

-- Descobre o numero de regioes
countRegions :: Grid -> Int
countRegions grid = countRegions' (getGridAsList grid) 0
countRegions' :: GridAsList -> Int -> Int
countRegions' grid count = (maximum regionsIndex) + 1 where regionsIndex = map (\x -> getRegion x) grid

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
toRegionsList grid = toRegionsList' grid 0 0 ((getLenghtGrid grid)-1) (createEmptyRegionsList (countRegions grid))
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

getDecidedValues :: Region -> [Int]
getDecidedValues (index, cells) = map (\y -> head y) (filter (\x -> length x == 1) cells)