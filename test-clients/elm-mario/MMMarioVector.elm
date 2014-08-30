module MMMarioVector where

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

-- ゼロベクトル
zeroVec : Vec
zeroVec = (0, 0)

-- 単位ベクトル
unitVec : Vec
unitVec = (1, 1)

{-| ベクトルの足し算

    addVec (1, 1) (2, 2) == (3, 3)
 -}
addVec : Vec -> Vec -> Vec
addVec (x, y) (mx, my) = (x + mx, y + my)

-- ベクトルの引き算
subVec : Vec -> Vec -> Vec
subVec (x, y) (mx, my) = (x - mx, y - my)

-- ベクトルに係数をかける
multVec : Float -> Vec -> Vec
multVec k (x, y) = (x * k, y * k)

-- ベクトルの内積
dotVec : Vec -> Vec -> Float
dotVec (x, y) (ax, ay) = x * ay + y * ax

-- ベクトルの反転
negVec : Vec -> Vec
negVec (x, y) = (-x, -y)

-- 長さ
lenVec : Vec -> Float
lenVec (x, y) = sqrt <| x*x + y*y

-- x要素
getx = fst
getw = fst

-- y要素
gety = snd
geth = snd

-- ベクトルをクランプする
clampVec (minX, minY) (maxX, maxY) (x, y) = (clamp minX maxX x, clamp minY maxY y)

-- ================================================================
-- レクト関係の関数
-- ================================================================

getOriginX rect = getx rect.origin
getOriginY rect = gety rect.origin

getSizeW rect = getx rect.size
getSizeH rect = gety rect.size

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

-- レクト同士で重なり合っている部分を検出
-- @see http://noriok.hatenablog.com/entry/2012/02/19/233543
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
