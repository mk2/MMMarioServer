module MMMarioVector where

type Vec = (Float, Float)

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

