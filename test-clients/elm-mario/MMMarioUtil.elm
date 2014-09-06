module MMMarioUtil where

import MMMarioVector (..)
import MMMarioType (..)
import MMMarioConfig (..)
import Array
import String (split, show)

-- lのリストをnつずつのリストに分解する
takeCycle n l =  takeCycle' n l []
takeCycle' n l accum =
    let notEnough = (length l) < n
    in  if | notEnough -> accum
           | otherwise -> takeCycle' n (drop n l) (accum ++ [take n l])

-- round関数とabs関数を一度に
absRound = round . abs


convPosToSend {pos} =
    "M" ++ (show . absRound . getx <| pos) ++ "," ++ (show . absRound . gety <| pos)

convRecvToCharas recvData =
    let poss = split "," recvData
        numCharas = (length poss) `div` 2
        maybeFloat = maybe 0.0 (\n -> n) . String.toFloat
        cnvToFloat = \[name, strX, strY] -> (name, (maybeFloat strX, maybeFloat strY))
    in zip [1 .. numCharas] <| map cnvToFloat <| takeCycle 3 poss
