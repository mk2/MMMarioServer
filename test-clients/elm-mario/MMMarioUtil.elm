module MMMarioUtil where

import MMMarioVector (..)
import MMMarioType (..)
import MMMarioConfig (..)
import Array
import String (split, show, toFloat)

-- lのリストをnつずつのリストに分解する
takeCycle n l =  takeCycle' n l []
takeCycle' n l accum =
    let notEnough = (length l) < n
    in  if | notEnough -> accum
           | otherwise -> takeCycle' n (drop n l) (accum ++ [take n l])

-- round関数とabs関数を一度に
absRound = round . abs


convPosToSend : Chara -> String
convPosToSend m =
    "M" ++ (show . absRound . getx <| m.rect.origin) ++ "," ++ (show . absRound . gety <| m.rect.origin)

convRecvToCharas recvData =
    let poss = split "," recvData
        numCharas = (length poss) `div` 2
        maybeFloat = maybe 0.0 (\n -> n) . toFloat
        cnvToFloat = \[name, strX, strY] -> (name, (maybeFloat strX, maybeFloat strY))
    in zip [1 .. numCharas] <| map cnvToFloat <| takeCycle 3 poss
