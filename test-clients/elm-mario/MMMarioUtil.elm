module MMMarioUtil where

import Array
import String (split, show, toFloat)

import MMMarioVector (..)
import MMMarioType (..)
import MMMarioConfig (..)

{-|
     色々なところで使うユーティリティ関数

    # リスト周り
    @doc takeCycle

    # WebSocket通信周り
 -}

{-|
    あるリストをnつずつのリストに分解する

    takeCycle 2 [1, 2, 3, 4, 5] == [[1, 2], [3, 4]]
    takeCycle 2 [1, 2, 3, 4, 5, 6] == [[1, 2], [3, 4], [5, 6]]
 -}
takeCycle n l =  takeCycle' n l []
takeCycle' n l accum =
    let notEnough = (length l) < n
    in  if | notEnough -> accum
           | otherwise -> takeCycle' n (drop n l) (accum ++ [take n l])

{-|
    キャラのレクトを文字列化
 -}
convCharaToSend : Chara -> String
convCharaToSend m =
    "MOV " ++ (rectToString m.rect)


{-|
    受信データをブロック（レクト）に変換

    convRecvToBlock "BLK R20,20,20,20" == { origin = (20,20), size = (20,20) }
 -}
convRecvToBlock : String -> Rect
convRecvToBlock recvData =
    let tokens = split " " recvData
        iden = head tokens
        r = rectstr . last <| tokens
    in r

{-|
    受信データをキャラクタータイプに変換
    2つ目の引き数に自キャラの名前を入れることで取り除ける

    convRecvToCharas "REC 1000 R20,20,20,20 2000 R30,30,30,30" 1000 ==
        [{rect = {rect = {origin=(30,30), size=(30,30)}}]
 -}
convRecvToCharas : String -> String -> [Chara]
convRecvToCharas recvData clientName =
    let tokens = split " " recvData
        iden = head tokens
        charaTupleList = filter (\[name, _] -> name /= clientName) . takeCycle 2 . tail <| tokens -- [[name, rect]]
    in map charaTupleToChara charaTupleList

{-|
    キャラのタプルをキャラタイプに変換

    charaTupleToChara ["100", "R20,20,20,20"] == { rect = {origin=(20,20), size=(20,20)}}
 -}
charaTupleToChara [name, rectStr] =
    let charaRect = rectstr rectStr
    in { defaultChara | rect <- charaRect }
