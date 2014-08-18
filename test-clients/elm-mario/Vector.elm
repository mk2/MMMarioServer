module Vector where

type Vec = (Float, Float)

zeroVec = (0, 0)

-- ベクトルの足し算
addVec : Vec -> Vec -> Vec
addVec (x, y) (mx, my) =
  (x + mx, y + my)

-- ベクトルの引き算
subVec : Vec -> Vec -> Vec
subVec (x, y) (mx, my) =
  (x - mx, y - my)

-- ベクトルに係数をかける
multVec : Vec -> Float -> Vec
multVec (x, y) k =
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
unitVec : Vec
unitVec = (1, 1)
