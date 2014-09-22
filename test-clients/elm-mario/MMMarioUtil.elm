module MMMarioUtil where

import Array
import String (split, show, toFloat)

import MMMarioVector (..)
import MMMarioType (..)
import MMMarioConfig (..)

{-| 色々なところで使うユーティリティ関数

# リスト周り
@doc takeCycle

# WebSocket通信周り
 -}

{-| あるリストをnつずつのリストに分解する

    takeCycle 2 [1, 2, 3, 4, 5] == [[1, 2], [3, 4]]
    takeCycle 2 [1, 2, 3, 4, 5, 6] == [[1, 2], [3, 4], [5, 6]]
 -}
takeCycle n l =  takeCycle' n l []
takeCycle' n l accum =
    let notEnough = (length l) < n
    in  if | notEnough -> accum
           | otherwise -> takeCycle' n (drop n l) (accum ++ [take n l])

convPosToSend : Chara -> String
convPosToSend m =
    "M" ++ (show . absRound . getx <| m.rect.origin) ++ "," ++ (show . absRound . gety <| m.rect.origin)

convRecvToCharas recvData =
    let poss = split "," recvData
        numCharas = (length poss) `div` 2
        maybeFloat = maybe 0.0 (\n -> n) . toFloat
        cnvToFloat = \[name, strX, strY] -> (name, (maybeFloat strX, maybeFloat strY))
    in zip [1 .. numCharas] <| map cnvToFloat <| takeCycle 3 poss
