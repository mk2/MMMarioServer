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

-- ゲームのFPS
gameFps = 60

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

maxBlockSizeWidth = 100
maxBlockSizeHeight = 100
minBlockSizeWidth = 50
minBlockSizeHeight = 50

-- 最大速度
maxSpd = (10, 10)
minSpd = (-10, -10)

-- 標準のキャラクター
defaultChara : Chara
defaultChara = {
                 rect = unitRect
               , spd = zeroVec
               , isTouchOnTopBlock = False
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
                     mario = { defaultChara | rect <- { origin = (0, 100), size = (20, 32) }
                                            , imageBaseName <- "mario"
                                            , imagePoseName <- "stand"
                                            , imageDireName <- "right"
                                            }
                   , sendData = ""
                   , blocks = []
                   , otherCharas = []
                   , clientName = ""
                   , stageSize = (400, 400)
                   , windowDims = (400, 400)
                   }

-- マリオのジャンプ移動速度
marioJumpStep = (0, 2000)

-- 重力加速度
gravityStep = (0, -900)

-- 摩擦係数
fricCoeff = 10

-- マリオ移動加速度係数
moveCoeff = 500