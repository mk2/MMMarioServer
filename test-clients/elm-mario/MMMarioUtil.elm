module MMMarioUtil where

import MMMarioType (..)
import MMMarioConfig (..)
import Array

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
getTileXCoord posX =
    posX `div` tileWidth

-- タイルのY座標を求める
getTileYCoord posY =
    posY `div` tileHeight

-- タイルのX座標にあるStageTileを求める
getStageTile stageTileRows xcoord ycoord =
    let
        stageTileRowsArray = Array.fromList stageTileRows
        stageTileRow = maybe [None] (\n -> n) <| Array.get ycoord stageTileRowsArray -- Maybe [StageTile]が返ってくる
        stageTileRowArray = Array.fromList stageTileRow
        stageTile = maybe None (\n -> n) <| Array.get xcoord stageTileRowArray -- Maybe StageTileが返ってくる
    in stageTile
