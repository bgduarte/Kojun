-- CONTEM O TIPO REGION LIST E TODAS AS FUNCOES NECESSARIAS PARA MANIPULALOS
module Modules.Regionslist where
import Modules.Cell

type PossibleCellValues = [Int]
type CoorX = Int
type CoorY = Int
type SolverCell = (PossibleCellValues, CoorX, CoorY, RegionIndex)
type Region = (RegionIndex, [SolverCell]) 
type RegionsList = [Region]


-- Funcao que converte uma regionList em uma lista de celulas
regionListToCellList :: RegionsList -> [SolverCell]
regionListToCellList [] = []
regionListToCellList ((_, x):xs) = x ++ regionListToCellList xs

-- ============================================================================================
    -- Funcoes de celula
-- ============================================================================================

-- Funcao que retorna o os valores possiveis dada uma celula
getPossibleValues :: SolverCell -> [Int]
getPossibleValues (possibleValues, _, _, _) = possibleValues

-- Dada uma celula retorna o indice da regiao na qual ela esta inserida
getRegionIndex :: SolverCell -> RegionIndex
getRegionIndex (possibleCellValues, coorX, coorY, regionIndex) = regionIndex

-- Retorna a celula que está acima da celula dada
getCellAbove :: SolverCell -> RegionsList -> SolverCell
getCellAbove (possibleCellValues, x, y, regionIndex) list = getCellFromCoord (x-1) y list

-- Retorna a celula que está abaixo da celula dada
getCellBelow :: SolverCell -> RegionsList -> SolverCell
getCellBelow (possibleCellValues, x, y, regionIndex) list = getCellFromCoord (x+1) y list

-- Retorna a celula que está a esquerda da celula dada
getCellLeft :: SolverCell -> RegionsList -> SolverCell
getCellLeft (possibleCellValues, x, y, regionIndex) list = getCellFromCoord x (y-1) list

-- Retorna a celula que está a direita da celula dada
getCellRight :: SolverCell -> RegionsList -> SolverCell
getCellRight (possibleCellValues, x, y, regionIndex) list = getCellFromCoord x (y+1) list

-- Retorna a celula que está na posicao x e y dada
getCellFromCoord :: Int -> Int -> RegionsList -> SolverCell
getCellFromCoord x y regions = 
    if getCellFromCoord' x y regions == [] then
         ([], -1, -1, -1) 
    else 
        (head $ getCellFromCoord' x y regions)
getCellFromCoord' :: Int -> Int -> RegionsList -> [SolverCell]
getCellFromCoord' x y regions = filter (\(possibleCellValues, x', y', regionIndex) -> x == x' && y == y' ) (regionListToCellList regions)

-- ============================================================================================
    -- Funcoes de regiao
-- ============================================================================================
-- Retorna uma regiao a partir de um indice
getRegionByIndex :: RegionIndex -> RegionsList -> Region
getRegionByIndex index regions = head $ filter (\(regionIndex, _) -> regionIndex == index) regions

-- Retorna a quantidade de celulas de uma regiao
getRegionSize :: Region -> Int
getRegionSize (_, cells) = length cells

--Retorna o indice de uma regiao
getIndex :: Region -> Int
getIndex (index, _) = index

-- ============================================================================================
    -- Funcoes de print
-- ============================================================================================

--Funcao para printar a lista de regioes (DEBUG)
getPrintRegions :: RegionsList -> String
getPrintRegions [] = ""
getPrintRegions (h:tail) = getPrintRegion h ++ "\n"++ getPrintRegions tail

--Funcao auxilar de getPrintRegions que printa uma unica regiao
getPrintRegion :: Region -> String
getPrintRegion (index, cells) = ("Regiao " ++ show index ++ ":" ++ show cells)