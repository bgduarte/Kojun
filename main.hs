import Modules.Grid
import Modules.GridResolver

main = do
    printGrid (grid 0)
    putStr (getPrintablePossibleValues (buildPossibleValues (grid 0)))
    print (calculateRegionsListSize (getGridAsList (grid 0)))