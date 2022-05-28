module Modules.Cell where 

type Region = Int
type Value = Int
--Regiao no qual pertence, valor, acima, abaixo, esquerda, direita
type Cell = (Region, Value, Cell, Cell, Cell, Cell)

getRegion :: Cell -> Region
getRegion (region, v, a, b, l, r) = region

setRegion :: Cell -> Int -> Cell
setRegion (_, v, a, b, l, r) region = (region, v, a, b, l, r)

getValue :: Cell -> Value
getValue (region, v, a, b, l, r) = v

setValue :: Cell -> Int -> Cell
setValue (region, _, a, b, l, r) v = (region, v, a, b, l, r)

getAbove :: Cell -> Cell
getAbove (region, v, a, b, l, r) = a

