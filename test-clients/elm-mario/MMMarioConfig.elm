module MMMarioConfig where

import MMMarioType (..)
import MMMarioVector (..)

{--================================================================--}
{-- Config --}
{--================================================================--}

-- 背景色
bgColor = rgb 160 216 239

-- 雲の色
cldColor = rgb 255 255 255

-- 土の色
gndColor = rgb 188 118 71

linColor = rgb 200 200 200

-- ゲームのFPS
gameFps = 0.5

-- サーバーへの送信FPS
-- 上げるとすぐ死ぬので注意
requestFps = 60

-- マリオ画像の幅(px)
marioImageWidth = 20

-- マリオ画像の高さ(px)
marioImageHeight = 35

-- リソースへのアクセスパス
resourceBaseUrl = "resources/"

imageBaseUrl = resourceBaseUrl ++ "images/"

-- 最大位置
maxPos = (640, 640)
minPos = zeroVec

maxBlockSizeWidth = 150
maxBlockSizeHeight = 150
minBlockSizeWidth = 25
minBlockSizeHeight = 25

stageWidth = 640
stageHeight = 640

-- 最大速度
maxSpd = (10, 10)
minSpd = (-10, -10)

-- 標準のキャラクター
defaultChara : Chara
defaultChara = {
                 rect = unitRect
               , spd = zeroVec
               , isTouchOnUpsideBlock = False
               , isTouchOnLeftBlock = False
               , isTouchOnDownBlock = False
               , isTouchOnRightBlock = False
               , mass = 100
               , imageBaseName = ""
               , imagePoseName = ""
               , imageDireName = ""
               }

-- ゲームの初期状態
initialGameState : GameState
initialGameState = {
                     self = { defaultChara | rect <- { origin = (0, 100), size = (20, 32) }
                                           , imageBaseName <- "mario"
                                           , imagePoseName <- "stand"
                                           , imageDireName <- "right"
                                           }
                   , sendData = ""
                   , ellapsedSeconds = 0.0
                   , blocks = []
                   , otherCharas = []
                   , clientName = ""
                   , stageSize = (400, 400)
                   , windowDims = (400, 400)
                   }

-- キャラのジャンプ移動速度
jumpStep = (0.0, 2000.0)

-- 重力加速度
gravityStep = (0.0, -900.0)

-- 摩擦係数
fricCoeff = 10.0

-- マリオ移動加速度係数
moveCoeff = 500.0