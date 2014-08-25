module MMMarioVector where

{--
ベクター関連のモジュール
Rect系もここに押し込んどく
 --}

-- ================================================================
-- 型宣言
-- ================================================================

type Vec = (Float, Float)

type Rect = { origin : Vec
            , size : Vec }

-- ================================================================
-- ベクトル関係の関数
-- ================================================================

-- ゼロベクトル
zero : Vec
zero = (0, 0)

-- ベクトルの足し算
addVec : Vec -> Vec -> Vec
addVec (x, y) (mx, my) =
  (x + mx, y + my)

-- ベクトルの引き算
subVec : Vec -> Vec -> Vec
subVec (x, y) (mx, my) =
  (x - mx, y - my)

-- ベクトルに係数をかける
multVec : Float -> Vec -> Vec
multVec k (x, y) =
  (x * k, y * k)

-- ベクトルの内積
dotVec : Vec -> Vec -> Float
dotVec (x, y) (ax, ay) =
  x * ay + y * ax

-- ベクトルの反転
revVec : Vec -> Vec
revVec (x, y) =
  (-x, -y)

-- 単位ベクトル
unit : Vec
unit = (1, 1)


-- x要素
getx = fst

-- y要素
gety = snd

-- ベクトルをクランプする
clampVec minVec maxVec vec =
    let x = getx vec
        y = gety vec
        maxX = getx maxVec
        maxY = gety maxVec
        minX = getx minVec
        minY = gety minVec
    in (clamp minX maxX x, clamp minY maxY y)

-- ================================================================
-- レクト関係の関数
-- ================================================================

-- サイズゼロのレクト
zeroRect : Rect
zeroRect = { origin = (0.0, 0.0), size = (0.0, 0.0) }

-- レクトを移動
moveRect : Vec -> Rect -> Rect
moveRect vec rect =
    { rect | origin <- addVec vec rect.origin }

-- レクトをリサイズ
resizeRect : Vec -> Rect -> Rect
resizeRect size rect =
    { rect | size <- size }

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
