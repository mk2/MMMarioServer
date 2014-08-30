module MMMarioUtil where

import MMMarioType (..)
import MMMarioConfig (..)
import Array

-- lのリストをnつずつのリストに分解する
takeCycle n l =  takeCycle' n l []
takeCycle' n l accum =
    let notEnough = (length l) < n
    in  if | notEnough -> accum
           | otherwise -> takeCycle' n (drop n l) (accum ++ [take n l])

-- round関数とabs関数を一度に
absRound = round . abs
