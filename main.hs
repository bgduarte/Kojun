import Modules.Grid
import Modules.GridResolver

iterator :: RegionsList -> Int -> [RegionsList]
iterator regionsList 0 = []
iterator regionsList n = regionsList : iterator (resolveFromRegionList regionsList) (n - 1)

teste list = check (([1,2,3,4],3,0,4) ,list)

main = do
    printGrid (grid 0)
    --putStr (getPrintRegions (iterator (toRegionsList (grid 0)) 6))
    let lists = (iterator (toRegionsList (grid 0)) 16)
    
    let listString = map (getPrintRegions) lists
    mapM_ putStr  listString
    let grids = map (\x -> (fromRegionsListToGrid x 6)) lists
    mapM_ printGrid grids

    --print ((teste (head (reverse lists))))
    --print (getCellFromCoord 0 2 (resolveFromRegionList (toRegionsList (grid 0))))
    --print (([1,2,3,4,5,6,7,8,9],0,2,1),(toRegionsList (grid 0)))
    --print (excludeForAdjacentCells (([1,2],0,5,2), (toRegionsList (grid 0))))

