module MMMarioConfig where

import MMMarioType (..)
import MMMarioVector (..)

{--================================================================--}
{-- Config --}
{--================================================================--}

-- サンプルマップ
sampleStageTiles : [[StageTile]]
sampleStageTiles = [(repeat 11 Ground)] ++ (repeat 9 . repeat 10 <| None)

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

-- タイルの幅(px)
tileWidth = 64

-- タイルの高さ(px)
tileHeight = 64

-- マリオ画像の幅(px)
marioImageWidth = 20

-- マリオ画像の高さ(px)
marioImageHeight = 35

-- リソースへのアクセスパス
resourceBaseUrl = "resources/"

imageBaseUrl = resourceBaseUrl ++ "images/"

-- 最大位置
maxPos = (640, 640)
minPos = (0, 0)

-- 最大速度
maxSpd = (5, 5)
minSpd = (-5, -5)

-- 標準のキャラクター
defaultChara : Chara
defaultChara = {
                 pos = zero
               , spd = zero
               , acc = zero
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
                     mario = { defaultChara | pos <- (0, 100)
                                            , imageBaseName <- "mario"
                                            , imagePoseName <- "stand"
                                            , imageDireName <- "right"
                                            }
                   , stageTileWidth = 200
                   , stageTileHeight = 100
                   , stageTiles = sampleStageTiles
                   , screenTileWidth = 10
                   , screenTileHeight = 10
                   , sendData = ""
                   , otherCharas = []
                   , clientName = ""
                   }

-- マリオのジャンプ加速度
marioJumpAccel = (0, 2000)

-- 重力加速度
gravityAccel = (0, -900)

-- 摩擦係数
fricCoeff = 10

-- マリオ移動加速度係数
moveCoeff = 500