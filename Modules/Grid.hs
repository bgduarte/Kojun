-- Contem o tipo grid e todas as declaracoes de grid
-- Tambem possui uma funcao de calcular o seu tamanho
-- e uma seria de funcoes parar imprimir a grid
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
        [( 0, 3),( 0, 0),( 1, 7),( 1, 0),( 2, 0),( 3, 3),( 3, 0),( 3, 5),( 4, 0),( 4, 6)],
        [( 0, 0),( 0, 2),( 1, 0),( 5, 0),( 2, 0),( 2, 0),( 3, 0),( 3, 0),( 4, 2),( 4, 0)],
        [( 1, 6),( 1, 4),( 1, 0),( 5, 0),( 2, 0),( 3, 6),( 3, 0),( 6, 0),( 4, 0),( 4, 3)],
        [( 1, 3),( 7, 0),( 5, 4),( 5, 0),( 2, 3),( 2, 0),( 2, 4),( 6, 0),( 6, 4),( 8, 0)],
        [( 7, 7),( 7, 1),( 7, 0),( 9, 0),( 9, 1),(10, 0),(10, 1),( 6, 0),( 6, 0),( 8, 0)],
        [(11, 1),(11, 0),( 7, 0),( 7, 0),( 7, 5),(12, 1),(13, 4),( 6, 1),( 6, 0),(14, 0)],
        [(15, 2),(15, 0),(15, 0),(16, 0),(16, 0),(16, 0),(13, 2),(13, 0),(17, 6),(14, 0)],
        [(18, 0),(19, 0),(19, 0),(16, 3),(20, 0),(16, 5),(13, 0),(17, 0),(17, 4),(14, 0)],
        [(18, 4),(21, 0),(18, 5),(16, 0),(20, 0),(22, 0),(17, 5),(17, 0),(17, 0),(14, 0)],
        [(18, 1),(18, 7),(18, 0),(18, 0),(22, 2),(22, 0),(22, 7),(22, 0),(22, 1),(22, 6)]
        ]
grid n = []

-- retorna o tamanho da grid
getLenghtGrid :: Grid -> Int
getLenghtGrid [] = 0
getLenghtGrid (x:xs) = 1 + getLenghtGrid xs

-- ========================================================================
-- Funcoes que printam uma dada grid
-- ========================================================================
-- funcao principal que printa a grid
printGrid :: Grid -> IO ()
printGrid grid = do 
    putStr (getPrintableGrid grid)

-- retorna uma string printavel da grid
getPrintableGrid :: Grid -> String
getPrintableGrid grid = getPrintableGrid' grid 0 0 ((getLenghtGrid grid))

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