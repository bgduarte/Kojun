-- Principal modulo de resolucao 
-- Contem as funcoes que resolvem a grid
-- Resolve a grid baseada numa lista de regioes
-- Utiliza do modulo converter para converter a grid para uma lista de listas e vice versa

module Modules.Solver where
import Modules.Grid
import Modules.Regionslist
import Modules.Converter


-- ======================================================================================================================
-- SolverCellMessage type e funcoes de acesso
-- ======================================================================================================================
type SolverCellMessage = (SolverCell, RegionsList)

getListFromSolverMessage :: SolverCellMessage -> RegionsList
getListFromSolverMessage (_, list) = list

getCellFromCellMessage :: SolverCellMessage -> SolverCell
getCellFromCellMessage (cell, _) = cell

-- ==========================================================================================
-- Funcao principal que recebe uma grid e retorna uma lista de grids com os passos da resolucao
-- ==========================================================================================
solve :: Grid -> [Grid]
solve grid = map (\x -> fromRegionsListToGrid x (getLenghtGrid grid)) (solveFromRegionList(initSolveRegions(toRegionsList grid)))

-- ==========================================================================================
    -- Funcoes que realizam a resolucao inicial (poe como valores inicias das celulas como [1..tamanho da regiao])
-- ==========================================================================================
--  Chama a resolucao inicia para todas as regioes da grid
initSolveRegions :: RegionsList -> RegionsList
initSolveRegions regions = (map (\x -> initSolveRegion x regions) regions)

-- Chama a resolucao incial para todas as celulas de uma regiao
initSolveRegion :: Region -> RegionsList -> Region
initSolveRegion (regionIndex, cells) regions = (regionIndex, (map (\x -> (initSolveCell x regions))) cells)

-- Inicializa os valores de todas as celulas todas as celulas 
initSolveCell :: SolverCell -> RegionsList -> SolverCell
initSolveCell (possibleCellValues, x, y, regionIndex) list = 
    if length possibleCellValues == 1 then 
        (possibleCellValues, x, y, regionIndex)
    else 
        getCellFromCellMessage(excludeByRegionSize ((possibleCellValues, x, y, regionIndex), list))

-- Poe como valores possivel para uma celula: [1.. tamanho da regiao]
excludeByRegionSize :: SolverCellMessage -> SolverCellMessage
excludeByRegionSize ((possibleCellValues, x, y, regionIndex), list) = updateRegionsList ((newPossibleValues, x, y, regionIndex), list )
    where newPossibleValues = [1..(getRegionSize(getRegionByIndex regionIndex list))]


-- ============================================================================================================================
-- Funcoes de que resolvem a lista de regioes
-- ============================================================================================================================

-- Funcao principal que resolve e retorna uma lista com todos os passos da resolucao
solveFromRegionList :: RegionsList -> [RegionsList]
solveFromRegionList list = 
    if notSolved list then
        list: (solveFromRegionList (solveFromRegionsList' list list))
    else
        [list]

-- Funcao que verifica se a lista de regioes esta resolvida
notSolved :: RegionsList -> Bool
notSolved list = length cellsNonSolved /= 0 where
    cellsNonSolved = (filter( \x -> (length x > 1)) allCellsPossibleValues)
    allCellsPossibleValues = (map (\x -> getPossibleValues x) allCells)
    allCells = (regionListToCellList list)


-- Funcao auxilia que resolve as lista de regioes chamando a funcao que resolve uma regiao para cada regiao
solveFromRegionsList' :: RegionsList -> RegionsList -> RegionsList
solveFromRegionsList' [] _ = [] 
solveFromRegionsList' (h:tail) holeList = ((solveRegion h holeList) : (solveFromRegionsList' tail holeList))


-- Resolva uma regiao chamando a funcao que resolve suas celulas
solveRegion :: Region -> RegionsList -> Region
solveRegion (regionIndex, cells) list = (regionIndex, solveCells cells list)

-- Para cada celula chama a funcao que a resolve
solveCells :: [SolverCell] -> RegionsList ->[SolverCell]
solveCells [] list = []
solveCells (h:tail) list = (solveCell h list) : (solveCells tail list)

-- Resolve uma celula chamando a funcao que resolve suas possibilidades
solveCell :: SolverCell -> RegionsList -> SolverCell
solveCell (possibleCellValues, x, y, regionIndex) list = 
    if length possibleCellValues == 1 then 
        (possibleCellValues, x, y, regionIndex)
    else 
        getCellFromCellMessage(solveCell' ((possibleCellValues, x, y, regionIndex),  list))

-- Resolve as possibilidades de uma celula chamando todas as funcoes auxiliares de resolucao de celula
solveCell' :: SolverCellMessage -> SolverCellMessage
solveCell' message = checkForOnlyCellPossibleForAValue(excludeForAboveInSameRegion(excludeForBelowInSameRegion (excludeForAdjacentCells (excludeForOtherCellsInRegion message) ))) -- . excludeForBelowInSameRegion . excludeForAboveInSameRegion

-- ============================================================================================================================
-- Funcaos auxiliares usadas para resolver a celula
-- ============================================================================================================================
-- Exclui das possibilidades os valores ja preenchidos em outras celulas da regiao
excludeForOtherCellsInRegion :: SolverCellMessage -> SolverCellMessage
excludeForOtherCellsInRegion ((possibleCellValues, x, y, regionIndex), list) = updateRegionsList ((cellValues , x, y, regionIndex), list) 
    where cellValues = (filter (`notElem` getDecidedValues ((getRegionByIndex regionIndex) list))) possibleCellValues

-- Exclui das possibilidades os valores ja preenchidos em celulas adjacentes
excludeForAdjacentCells :: SolverCellMessage -> SolverCellMessage
excludeForAdjacentCells ((possibleCellValues, x, y, regionIndex) , list) = updateRegionsList ((cellValues , x, y, regionIndex), list) where
    cellValues = (filter (`notElem` (getAdjacentCellValues ((possibleCellValues, x, y, regionIndex), list))) possibleCellValues)

-- Exclui das possibilidades os valores menores ou iguais que o menor valor da celula abaixo
excludeForBelowInSameRegion :: SolverCellMessage -> SolverCellMessage
excludeForBelowInSameRegion ((possibleCellValues, x, y, regionIndex), list) = updateRegionsList ((cellValues , x, y, regionIndex), list) where
    cellValues = 
        if length belowCellValues > 0 && (getRegionIndex cellBelow == regionIndex) then 
            filter (> (minimum belowCellValues)) possibleCellValues
        else
            possibleCellValues
    belowCellValues = ( getPossibleValues cellBelow)
    cellBelow = getCellBelow (possibleCellValues, x, y, regionIndex) list

-- Exclui das possibilidades os valores maiores ou iguais que o maior valor da celula acima
excludeForAboveInSameRegion :: SolverCellMessage -> SolverCellMessage
excludeForAboveInSameRegion ((possibleCellValues, x, y, regionIndex), list) = updateRegionsList ((cellValues , x, y, regionIndex), list) where
    cellValues = 
        if length aboveCellValues > 0 && (getRegionIndex cellAbove == regionIndex) then 
            filter (< (maximum aboveCellValues)) possibleCellValues
        else
            possibleCellValues
    aboveCellValues = ( getPossibleValues cellAbove)
    cellAbove = getCellAbove (possibleCellValues, x, y, regionIndex) list

-- Verifica se dentro das celulas da regiao esta eh a unica que eh possivel a atribuicao de um valor e , se sim, atribui esse valor a ela
checkForOnlyCellPossibleForAValue :: SolverCellMessage -> SolverCellMessage
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
-- ============================================================================================================================
-- Funcoes auxiliares a funcoes auxiliares usadas para resolver a celula
-- ============================================================================================================================

-- Retorna as celulas adjacentes da celula dada
getAdjacentCells :: SolverCellMessage -> [SolverCell]
getAdjacentCells (cell, regions) = [getCellRight cell regions] ++ [getCellLeft cell regions] ++ [getCellAbove cell regions] ++ [getCellBelow cell regions]

-- Retorna os valores das celulas celulas adjacentes
getAdjacentCellValues :: SolverCellMessage -> [Int]
getAdjacentCellValues (cell, regions) = adjValues  
    where
        adjValues = map (\x -> head x) adjValidValues
        adjValidValues = filter (\x -> length x == 1) adjValuesList
        adjValuesList = map (\(values, _, _, _) ->  values) (getAdjacentCells (cell, regions))

--Pega os valores em uma regiao que ja foram decididos
getDecidedValues :: Region -> [Int]
getDecidedValues (index, cells) = map (\x -> head x) decidedValues 
    where decidedValues = filter (\x -> (length x == 1)) possibleValues
          possibleValues = map (\x -> getPossibleValues x) cells

-- ============================================================================================================================
-- Funcoes que atualizam a lista de regions dada uma celula com novos valores
-- ============================================================================================================================
-- Funcao principal que chama a auxiliar
updateRegionsList :: SolverCellMessage -> SolverCellMessage
updateRegionsList (cell , list) = (cell , updateRegionsList' (cell, list) )

-- Acha a regiao correta e chama a funcao que a atualiza
updateRegionsList' :: SolverCellMessage -> RegionsList
updateRegionsList' (cell , []) = [] 
updateRegionsList' ((values, x, y, regionIndex) , (h:tail)) = 
    if (getIndex h) == regionIndex then 
       [(updateRegion h (values, x, y, regionIndex))] ++ (updateRegionsList' ((values, x, y, regionIndex) , tail))
    else 
        [(h)] ++ (updateRegionsList' ((values, x, y, regionIndex) , tail))

-- Atualiza a regiao dada com a nova celula chamando a funcao que atualiza a celula
updateRegion :: Region -> SolverCell -> Region
updateRegion (index, cells) (values, x, y, regionIndex) = (index, (updateCell (values, x, y, regionIndex) cells))

-- Atualiza a celula dada uma lista de celulas de uma regiao
updateCell :: SolverCell -> [SolverCell] -> [SolverCell]
updateCell (values, x, y, regionIndex) [] = []
updateCell (values, x, y, regionIndex) (h:tail) =
    if hasTheSameCoord h (values, x, y, regionIndex) then
        (values, x, y, regionIndex) : (updateCell (values, x, y, regionIndex) tail)
    else
        h : (updateCell (values, x, y, regionIndex) tail)

-- Funcao que verifica se duas celulasa possuem a mesma coordenada x,y
hasTheSameCoord :: SolverCell -> SolverCell -> Bool
hasTheSameCoord (values, x, y, regionIndex) (values2, x2, y2, regionIndex2) = (x == x2) && (y == y2)

