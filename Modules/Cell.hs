module Modules.Cell where 
type RegionIndex = Int
type Value = Int
type Cell  = (RegionIndex, Value)

getValue :: Cell -> Value
getValue (_, v) = v

getRegion :: Cell -> RegionIndex
getRegion (r, _) = r

getPrintableValue :: Cell -> String
getPrintableValue (r, 0) = " "
getPrintableValue (r, v) = show v