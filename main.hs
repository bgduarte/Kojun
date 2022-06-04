import Modules.Grid
import Modules.Solver

-- O modulo solver utiliza do modulo converter para converter a grid em uma lista de regioes
-- Resolve essa lista de regioes salvando todos os passo de resolucao
-- Converte de volta para grid com o converter e retorna

-- Tambem utiliza o modulo regionslist que contem o tipo utilizado para a resolucao e todas as funcoes relacionadas a este

main = do
    printGrid (grid 0)
    let grids = solve (grid 0)
    mapM_ printGrid grids

