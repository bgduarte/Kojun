-- Contem todas as funcoes necessarias para conversao de grid em lista de regioes e o contrario
module Modules.Converter where
import Modules.Cell
import Modules.Grid
import Modules.Regionslist

type GridAsList = [Cell]

-- ================================================================================================
-- Funcoes que converte uma Grid em uma lista de regioes
-- ================================================================================================

-- Funcao principal que a grid em uma lista de regioes que chama a auxilar passando uma lista de regioes vazia
toRegionsList :: Grid -> RegionsList
toRegionsList grid = (toRegionsList' grid 0 0 ((getLenghtGrid grid)-1) (createEmptyRegionsList (countRegions grid)))

-- Funcao auxilar que para cada linha da grid chama a funcao que a converte 
-- Grid -> x -> y -> lenght -> RegionsList
toRegionsList' :: Grid -> Int -> Int -> Int -> RegionsList -> RegionsList
toRegionsList' (h:tail) x y len regions = 
    if  x < len then 
        toRegionsList' tail (x+1) y len (toRegionListRow h x 0 len regions)
    else 
        toRegionListRow h x 0 len regions

--Funcao auxiliar a toRegionsList que converte uma linha da grid
toRegionListRow :: Row -> Int -> Int -> Int -> RegionsList -> RegionsList
toRegionListRow (h:tail) x y len regions = 
    if  y < len then 
        toRegionListRow tail x (y+1) len ((addToRegion (getRegion h) (makeSolverCell x y h) regions) )
    else 
        addToRegion (getRegion h) (makeSolverCell x y h) regions

--Cria a lista de regioes dada um tamanho de grid, porem vazia
createEmptyRegionsList :: Int -> RegionsList
createEmptyRegionsList count = createEmptyRegionsList' count []
createEmptyRegionsList' :: Int -> RegionsList -> RegionsList
createEmptyRegionsList' 0 list = list
createEmptyRegionsList' count list = createEmptyRegionsList' (count - 1) ((count-1, []):list)


-- Transforma a grid em uma lista para facilitar o acesso
getGridAsList :: Grid -> GridAsList
getGridAsList [] = []
getGridAsList (x:xs) = x ++ getGridAsList xs

-- Descobre o numero de regioes
countRegions :: Grid -> Int
countRegions grid = countRegions' (getGridAsList grid) 0
countRegions' :: GridAsList -> Int -> Int
countRegions' grid count = (maximum regionsIndex) + 1 where regionsIndex = map (\x -> getRegion x) grid

--Cria uma SolverCell baseada numa cell comum
makeSolverCell :: Int -> Int -> Cell -> SolverCell
makeSolverCell x y cell = ((getPossibleCellValues cell), x, y, getRegion cell)

-- Adiciona uma celula para regiao que que tem o indice que foi passado
--indice da regiao --> Celula -> Lista de regioes -> lista de regioes
addToRegion :: Int -> SolverCell -> RegionsList -> RegionsList
addToRegion index cell list = map (\x -> addCell x cell index) list

-- Adiciona uma celula em uma regia se o indice passado é igual ao indice da regiao
addCell :: Region -> SolverCell -> Int -> Region 
addCell (index, cells) cell i = 
    if i == index then 
        (index, cells ++ [cell])
    else 
        (index, cells)


-- ============================================================================================
-- Funcoes que convertem de RegionList para uma Grid
-- ============================================================================================
-- funcao principal que chama a auxiliar
fromRegionsListToGrid :: RegionsList -> Int -> Grid  
fromRegionsListToGrid list gridSize = fromRegionsListToGrid' list gridSize 0 0

-- funcao que chama a funcao que calcula as linhas
fromRegionsListToGrid' :: RegionsList -> Int -> Int -> Int -> Grid
fromRegionsListToGrid' list size x y =
    if x < size then 
        (fromRegionsListToGridRow list size x 0) : (fromRegionsListToGrid' list size (x+1) y)
    else
        []

-- funcao que calcula as linhas chamando a funcao que converte uma dada celula para um celula de grid
fromRegionsListToGridRow  :: RegionsList -> Int -> Int -> Int -> Row
fromRegionsListToGridRow list size x y = 
    if y < size then 
        getTrueCell(getCellFromCoord x y list) : (fromRegionsListToGridRow list size x (y+1))
    else
        []

--Converte uma celula de resolucao para uma celula da grid
getTrueCell :: SolverCell -> Cell
getTrueCell (values, _, _, regionIndex) = (regionIndex, value) 
    where value = if (length values) == 1 then head values else 0

--Preenhe a celula com todos os valores possiveis caso ela ja nao tem um valor definido
getPossibleCellValues :: Cell -> PossibleCellValues
getPossibleCellValues cell = 
    if getValue cell == 0 then 
        [1..9]
    else 
        [getValue cell]
