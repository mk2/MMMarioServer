module MMMarioUtil where

-- リストをタプルに。
-- ex: [1, 2] -> (1, 2)
list2tuple l = (head l, last l)

-- lのリストをnつずつのタプルのリストに分解する
takeCycle n l =  takeCycle' n l []
takeCycle' n l accum =
    let notEnough = (length l) < n
    in  if | notEnough -> accum
           | otherwise -> takeCycle' n (drop n l) (accum ++ [list2tuple <| (take n l)])

-- round関数とabs関数を一度に
absRound = round . abs
