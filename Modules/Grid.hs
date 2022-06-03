{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use guards" #-}
module Modules.Grid where
import Modules.Cell

type Row = [Cell]
type Grid = [Row]

grid :: Int -> Grid
grid 0 = [
    [(0,2), (0,0), (1,0), (1,0), (1,1), (2,0)],
    [(3,0), (3,0), (3,0), (3,3), (3,0), (2,0)],
    [(4,0), (5,3), (5,0), (5,0), (3,5), (6,3)],
    [(4,0), (4,0), (4,0), (5,0), (6,0), (6,0)],
    [(7,0), (7,0), (8,3), (9,0), (9,4), (9,2)],
    [(10,0), (10,0), (8,0), (8,0), (9,0), (9,0)]
    ]
grid 1 = [
        [(0,2), (0,1), (1,3), (1,2), (1,1), (2,2)],
        [(3,1), (3,4), (3,2), (3,3), (3,6), (2,1)],
        [(4,4), (5,3), (5,4), (5,2), (3,5), (6,3)],
        [(4,3), (4,1), (4,2), (5,1), (6,2), (6,1)],
        [(7,1), (7,2), (8,3), (9,5), (9,4), (9,2)],
        [(10,2), (10,1), (8,2), (8,1), (9,3), (9,1)]
        ]
grid n = []

-- retorna o tamanho da grid
getLenghtGrid :: Grid -> Int
getLenghtGrid [] = 0
getLenghtGrid (x:xs) = 1 + getLenghtGrid xs


-- retorna um elento da grid
getFromGrid :: Int -> Int -> Grid -> (RegionIndex, Value)
getFromGrid x y grid = grid !! x !! y


-- cria as linhas que reprentam a extrimadade da grid
makeExtremeLine :: Int -> String
makeExtremeLine 0 = "--"
makeExtremeLine n = "---" ++ makeExtremeLine (n-1)

-- cria a linha que fica entre as linhas da grid
makeLineSpace :: Row -> Row -> Int -> String
makeLineSpace [] [] _ = ""
makeLineSpace x y len = makeLineSpace' x y 0 len

-- ajudante de makeLineSpace' que calcula o character baseado nas regioes
calculateCharForLineSpace :: Cell -> Cell -> String 
calculateCharForLineSpace x y = if getRegion(x) == getRegion(y) then "   " else "---"

-- calcula a linha que fica entre as linhas da grid
makeLineSpace' :: Row -> Row -> Int -> Int -> String
makeLineSpace' [] [] _ _ = ""
makeLineSpace' [] _ _ _ = ""
makeLineSpace' _ [] _ _  = ""
makeLineSpace' (x:xs) (y:ys) n len =
    if n == 0 then 
        "\n|" ++ makeLineSpace' (x:xs) (y:ys) (n+1) len
    else if n < len then
        calculateCharForLineSpace x y ++ makeLineSpace' xs ys (n+1) len
    else
        calculateCharForLineSpace x y ++ "|\n"

-- calcula o caracter que fica entre as celulas baseado nas regioes
calculateCharForLine :: Cell -> Cell -> String
calculateCharForLine x y = if getRegion(x) == getRegion(y) then "` " else "| "

-- calcula a linha que representa o valor e regios das celulas
getPrintableRow :: Row -> Int -> Int -> Int -> String
getPrintableRow [] x y len = ""
getPrintableRow (h:tail) x y len = 
    if y == 0 then
        "| " ++ getPrintableRow (h:tail) x (y+1) len
    else if y < len then
        getPrintableValue h ++ calculateCharForLine h (head tail) ++ getPrintableRow tail x (y+1) len
    else 
        getPrintableValue h ++ " |"

-- calcula a string printavel da grid
getPrintableGrid' :: Grid -> Int -> Int -> Int -> String
getPrintableGrid' [] _ _ _ = ""
getPrintableGrid' (h:tail) x y len= 
    if x == 0 then 
        makeExtremeLine (len) ++ "\n" ++ getPrintableGrid' (h:tail) (x+1) 0 len
    else if x < len then
        getPrintableRow h x y len ++ makeLineSpace h (head tail) len ++ getPrintableGrid' tail (x+1) 0 len
    else 
        getPrintableRow h x y len ++ "\n" ++ makeExtremeLine (len) ++ "\n"

-- retorna uma string printavel da grid
getPrintableGrid :: Grid -> String
getPrintableGrid grid = getPrintableGrid' grid 0 0 ((getLenghtGrid grid))

-- printa a grid
printGrid :: Grid -> IO ()
printGrid grid = do 
    putStr (getPrintableGrid grid)

generateEmptyGrid :: Int -> Grid
generateEmptyGrid size = generateEmptyGrid' size 0 0

generateEmptyGrid' :: Int -> Int -> Int -> Grid
generateEmptyGrid' size x y =
    if x < size then
        []:generateEmptyGrid' size (x+1) y
    else
        []

generateEmptyRoll :: Int -> Int -> Int -> Row
generateEmptyRoll size x y = 
    if y < size then 
       (-1,0) : generateEmptyRoll size x (y+1)
    else
        []

