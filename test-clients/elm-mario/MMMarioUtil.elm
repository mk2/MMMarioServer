module MMMarioUtil where

import MMMarioConfig (..)

-- リストをタプルに。
-- ex: [1, 2] -> (1, 2)
list2tuple l = (head l, last l)

-- lのリストをnつずつのタプルのリストに分解する
takeCycle n l =  takeCycle' n l []
takeCycle' n l accum =
    let notEnough = (length l) < n
    in  if | notEnough -> accum
           | otherwise -> takeCycle' n (drop n l) (accum ++ [list2tuple <| (take n l)])

-- lのリストをnつずつのリストに分解する
takeCycleAsList n l =  takeCycleAsList' n l []
takeCycleAsList' n l accum =
    let notEnough = (length l) < n
    in  if | notEnough -> accum
           | otherwise -> takeCycleAsList' n (drop n l) (accum ++ [take n l])

-- round関数とabs関数を一度に
absRound = round . abs

-- タイルのX座標を求める
getTileXCoord pos =
    pos `div` tileWidth
getTileYCoord pos =
    pos `div` tileHeight
