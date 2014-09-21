module MMMarioVector where

import String as S
import Array
import Maybe

{-| ベクター関連のモジュール
    Rect系もここに押し込んどく

# 型
@docs Vec, Rect

# ベクトル計算
@docs addVec, subVec, multVec, dotVec, negVec, lenVec

# ベクトル要素取り出し
@docs getx, gety

# レクト計算
@docs moveRect, resizeRect

 -}

-- ================================================================
-- 型宣言
-- ================================================================

{-| ベクトル型。タプルで表現 -}
type Vec = (Float, Float)

{-| レクト型 -}
type Rect = {
              origin : Vec
            , size : Vec
            }

-- ================================================================
-- ベクトル関係の関数
-- ================================================================

{-| コンストラクタ。IntのタプルからVecを生成

    vec (1, 2) == (1.0, 2.0)
 -}
vec : (Int, Int) -> Vec
vec (x, y) = (toFloat x, toFloat y)

vecstr : (String, String) -> Vec
vecstr  (rawx, rawy) =
    let tofloat = \i -> maybe 0.0 (\j -> j) . S.toFloat
    in (tofloat rawx, tofloat rawy)

{-| ゼロベクトル -}
zeroVec : Vec
zeroVec = (0, 0)

{-| 単位ベクトル -}
unitVec : Vec
unitVec = (1, 1)

{-| ベクトルの足し算

    addVec (1, 1) (2, 2) == (3, 3)
 -}
addVec : Vec -> Vec -> Vec
addVec (x, y) (mx, my) = (x + mx, y + my)

{-| ベクトルの引き算

    subVec (3, 3) (1, 1) == (2, 2)
 -}
subVec : Vec -> Vec -> Vec
subVec (x, y) (mx, my) = (x - mx, y - my)

{-| ベクトルを係数倍

    multVec 3.0 (1, 2) == (3, 6)
 -}
multVec : Float -> Vec -> Vec
multVec k (x, y) = (x * k, y * k)

{-| ベクトルの内積を求める

    dotVec (2, 3) (3, 2) == 13
 -}
dotVec : Vec -> Vec -> Float
dotVec (x, y) (ax, ay) = x * ay + y * ax

{-| ベクトルを反転

    negVec (1, 1) == (-1, -1)
 -}
negVec : Vec -> Vec
negVec (x, y) = (-x, -y)

{-| ベクトルのX要素だけ反転
 -}
negxVec : Vec -> Vec
negxVec (x, y) = (-x, y)

{-| ベクトルのY要素だけ反転
 -}
negyVec : Vec -> Vec
negyVec (x, y) = (x, -y)

{-| ベクトルのX要素をゼロ化して返す
 -}
zeroxVec : Vec -> Vec
zeroxVec (x, y) = (0.0, y)

{-| ベクトルのY要素をゼロ化して返す
 -}
zeroyVec : Vec -> Vec
zeroyVec (x, y) = (x, 0.0)

{-| ベクトルの長さを求める

    lenVec (1, 1) == 1.4...
 -}
lenVec : Vec -> Float
lenVec (x, y) = sqrt <| x*x + y*y

{-| ベクトル座標が、右にあるかどうか

    isRightPos (3, 10) (5, 3) == True
 -}
isRightPos : Vec -> Vec -> Bool
isRightPos (bx, _) (x, _) = bx < x

{-| ベクトル座標が左にあるかどうか

    isLeftPos (6, 4) (2, 12) == True
 -}
isLeftPos : Vec -> Vec -> Bool
isLeftPos (bx, _) (x, _) = bx > x

{-| ベクトル座標が上にあるかどうか

    isUpsidePos (4, 10) (1, 12) == True
 -}
isUpsidePos : Vec -> Vec -> Bool
isUpsidePos (_, by) (_, y) = y > by

{-| ベクトル座標が下にあるかどうか

    isUpsidePos (4, 15) (1, 12) == True
 -}
isDownPos : Vec -> Vec -> Bool
isDownPos (_, by) (_, y) = y < by

{-| ベクトルのx要素を取得

    getx (3, 5) == 3
 -}
getx = fst

{-| ベクトルのx要素を取得

    getw (3, 5) == 3
 -}
getw = fst

{-| ベクトルのy要素を取得

    gety (3, 5) == 5
 -}
gety = snd

{-| ベクトルのy要素を取得

    geth (3, 5) == 5
 -}
geth = snd

{-| ベクトルをクランプする

    clampVec (0, 10) (100, 20) (-1, 15) == (0, 15)
 -}
clampVec (minX, minY) (maxX, maxY) (x, y) = (clamp minX maxX x, clamp minY maxY y)

-- ================================================================
-- レクト関係の関数
-- ================================================================

getOriginX : Rect -> Float
getOriginX rect = getx rect.origin
getOriginY : Rect -> Float
getOriginY rect = gety rect.origin

getSizeW : Rect -> Float
getSizeW rect = getx rect.size
getSizeH : Rect -> Float
getSizeH rect = gety rect.size

getArea : Rect -> Float
getArea {size} = uncurry (*) size

getRectCenter : Rect -> Vec
getRectCenter {origin, size} = addVec origin <| multVec 0.5 size

-- サイズゼロのレクト
zeroRect : Rect
zeroRect = { origin = (0.0, 0.0), size = (0.0, 0.0) }

-- 単位レクト
unitRect : Rect
unitRect = { origin = (1.0, 1.0), size = (1.0, 1.0) }

-- レクトを移動
moveRect : Vec -> Rect -> Rect
moveRect mvec rect = { rect | origin <- addVec mvec rect.origin }

-- レクトをリサイズ
resizeRect : Vec -> Rect -> Rect
resizeRect newsize rect = { rect | size <- newsize }

{-| レクトを文字列化
 -}
rectToString : Rect -> String
rectToString rect = "R"
     ++ (S.show . getx <| rect.origin)
     ++ "," ++ (S.show . gety <| rect.origin)
     ++ "," ++ (S.show . getx <| rect.size)
     ++ "," ++ (S.show . gety <| rect.size)

{-| 文字列からレクトを作成する
 -}
stringToRect : String -> Nothing Rect
stringToRect rawRectStr =
    let unconsRectStr = S.uncons rawRectStr
        createRect = \vals -> {origin = vecstr (Array.getOrElse "0" 0 vals) (Array.getOrElse "0" 1 vals),
                               size = vecstr (Array.getOrElse "0" 2 vals) (Array.getOrElse "0" 3 vals)}
    in case unconsRectStr of
        Nothing -> Nothing
        ('R', rectStr) -> createRect <| Array.fromList <| S.split "," rectStr

{-| レクトをベクトルでクランプ
 -}
clampRect : Vec -> Vec -> Rect -> Rect
clampRect minPos maxPos r =
    let ev = addVec r.origin r.size
        isUpside = isUpsidePos maxPos ev
        isRight = isRightPos maxPos ev
    in if | isUpside && isRight -> { r | origin <- subVec maxPos r.size }
          | isUpside && not isRight -> { r | origin <- ((getx r.origin), ((gety maxPos - gety r.size))) }
          | not isUpside && isRight -> { r | origin <- (((getx maxPos - getx r.size)), (gety r.origin)) }
          | otherwise -> { r | origin <- clampVec minPos maxPos r.origin }

{-| レクト同士で重なり合っている部分を検出
    @see http://noriok.hatenablog.com/entry/2012/02/19/233543

    getOverlapRect {origin=(0,0),size=(50,50)} {oring=(25,25),size=(30,30)} == Just {origin=(25,25),size=(5,5)}
    getOverlapRect {origin=(0,0),size=(50,50)} {oring=(51,51),size=(30,30)} == Nothing
 -}
getOverlapRect : Rect -> Rect -> Maybe Rect
getOverlapRect recta rectb =
    let
        ox = max (getx recta.origin) (getx rectb.origin)
        oy = max (gety recta.origin) (gety rectb.origin)
        ex = min ((getx recta.origin) + (getx recta.size)) ((getx rectb.origin) + (getx rectb.size))
        ey = min ((gety recta.origin) + (gety recta.size)) ((gety rectb.origin) + (gety rectb.size))
        w = ex - ox
        h = ey - oy
        isOverlap = w > 0 && h > 0
    in if | not isOverlap -> Nothing
          | otherwise -> Just { origin = (ox, oy)
                              , size = (w, h) }
