module MMMarioClient where

import Graphics.Input (Input, input, button)
import Window
import Keyboard
import Graphics.Collage (..)
import Graphics.Element (image, fittedImage, croppedImage)
import WebSocket
import Debug (log)
import String (split, show)

-- 自作ライブラリのimport
import Vector (..)

{--------------------------------------------------------------------}
{--
 設定値
 --}

-- ゲームのFPS
gameFps = 60

-- サーバーへの送信FPS
-- 上げるとすぐ死ぬので注意
requestFps = 1

-- リソースへのアクセスパス
resourceBaseUrl = "resources/"

-- 画像へのアクセスパス
imageBaseUrl = resourceBaseUrl ++ "images/"

maxPos = (1000, 1000)
minPos = (0, 0)

-- 標準のキャラクター
defaultChara = {
                 pos = zero
               , spd = zero
               , acc = zero
               , isTouchOnGround = False
               , isTouchOnBlock = False
               , mass = 100
               , imageBaseName = ""
               , imagePoseName = ""
               , imageDireName = ""
               }

-- ゲームの初期状態
initialGameState = {
                     mario = { defaultChara | pos <- (0, 100)
                                            , imageBaseName <- "mario"
                                            , imagePoseName <- "stand"
                                            , imageDireName <- "right"
                                            }
                   , stageTileWidth = 200
                   , stageTileHeight = 100
                   , screenTileWidth = 10
                   , screenTileHeight = 10
                   , sendData = ""
                   , otherCharas = []
                   }

-- マリオのジャンプ加速度
marioJumpAccel = (0, 10000)

-- 重力加速度
gravityAccel = (0, -10000)

-- 摩擦係数
fricCoeff = 500

-- マリオ移動加速度係数
moveCoeff = 200000

-- タイルの幅(px)
tileWidth = 32

-- タイルの高さ(px)
tileHeight = 32

{--------------------------------------------------------------------}
{--
 ゲームで使う型
 --}
data RenderType = RChara | RBlock | RItem

type Chara = {
               pos : Vec
             , spd : Vec
             , acc : Vec
             , isTouchOnBlock : Bool
             , isTouchOnGround : Bool
             , mass : Float
             , imageBaseName : String
             , imagePoseName : String
             , imageDireName : String
             }

type GameState = {
                   mario : Chara
                 , stageTileWidth : Float
                 , stageTileHeight : Float
                 , screenTileWidth : Float
                 , screenTileHeight : Float
                 , otherCharas : [(String, Chara)]
                 , sendData : String
                 }

type UserInput = {
                   arr : {x : Int, y : Int}
                 , space : Bool
                 }

-- キャラクターの加速度を計算
-- 計算方法
-- delta : t
-- moveAccel : 移動加速度
-- fricAccel : 摩擦加速度
-- gravityAccel : 重力加速度
-- willJump : ジャンプ状態に持っていくかどうか
calcCharaAccel delta moveAccel fricAccel gravityAccel willJump m =
  let
      -- キャラクターのx,y座標
      x = getx m.pos
      y = gety m.pos

      -- 各加速度はdelta秒分の量にする
      dGravityAccel = multVec gravityAccel delta
      dMoveAccel = log "dMoveAccel" <| multVec moveAccel delta
      dFricAccel = multVec fricAccel delta
      dSmallFricAccel = multVec dFricAccel 0.2

      -- ジャンプできるかどうか
      jumpable = willJump && m.isTouchOnGround

  in if | jumpable -> { m | acc <- addVec m.acc marioJumpAccel } -- ジャンプキーが押されてかつ地面に触れていた場合ジャンプ可能
        | not m.isTouchOnGround -> { m | acc <- addVec m.acc
                                             <| addVec dSmallFricAccel dGravityAccel } -- 地面に触れていない場合、移動できない
        | otherwise -> { m | acc <- log "accel" <| addVec m.acc
                                 <| addVec dGravityAccel
                                 <| addVec dMoveAccel dFricAccel } -- それ以外の場合（全ての加速度が現在の加速度にたされる）

-- キャラクターの位置を計算
calcCharaPos delta m =
  let x = getx m.pos
      y = gety m.pos
      ax = log "accelx" <| getx m.acc
      ay = log "accely" <| gety m.acc
      sx = ax * delta
      sy = ay * delta
  in if | m.isTouchOnBlock -> { m | acc <- zero, spd <- zero
                                  , isTouchOnGround <- True }
        | y < 0 -> { m | acc <- (ax, 0)
                       , pos <- clampVec minPos maxPos (x, 0)
                       , isTouchOnGround <- True }
        | otherwise -> { m | pos <- clampVec minPos maxPos <| addVec m.pos <| multVec m.spd delta
                           , spd <- (sx, sy)
                           , isTouchOnGround <- False }

-- キャラクターのイメージを更新
updateCharaImage m =
  m

-- キャラクターイメージの取得
getImage chara (w, h) =
  image w h (concat [imageBaseUrl, chara.imageBaseName, "-", chara.imagePoseName, "-", chara.imageDireName, ".png"])

-- リストをタプルに。
-- ex: [1, 2] -> (1, 2)
list2tuple l = (head l, last l)

-- lのリストをnつずつのタプルのリストに分解する
takeCycle n l =  takeCycle' n l []
takeCycle' n l accum =
    let notEnough = (length l) < n
    in  if | notEnough -> accum
           | otherwise -> takeCycle' n (drop n l) (accum ++ [list2tuple <| (take n l)])

-- ゲーム関数
-- (更新秒, (矢印キー上下, 矢印キー左右), キーボード) -> ゲームステート -> ゲームステート
stepGame : (Float, {x : Int, y : Int}, Bool, String) -> GameState -> GameState
stepGame (delta, arr, space, recvData) gameState =
  let
      -- 更新前のマリオ
      preMario = gameState.mario

      -- 移動加速度
      moveAccel = log "moveAccel" <| multVec (toFloat arr.x, toFloat arr.y) moveCoeff

      -- 摩擦加速度
      fricAccel = multVec (revVec preMario.spd) fricCoeff

      -- 更新関数
      updateChara = updateCharaImage . calcCharaPos delta . calcCharaAccel delta moveAccel fricAccel gravityAccel space

      -- マリオを更新する
      newMario = updateChara <| preMario

      -- 送信するマリオのいち情報
      marioPosStr = "M" ++ (show . getx <| newMario.pos) ++ "," ++ (show . gety <| newMario.pos)

      -- 別キャラの位置
      poss = split "," recvData
      numCharas = (length poss) `div` 2
      otherCharas = log "otherCharas" <| zip [1 .. numCharas] <| takeCycle 2 poss

  in { gameState | mario <- newMario
                 , sendData <- marioPosStr
     }

-- WebSocketからの受信データ
port wsRecvData : Signal String

-- 入力シグナル
-- delta更新毎にkeySignal (デルタ秒, (矢印キー), スペースキー, WSからの受信データ) を取得
inputSignal =
  let delta = inSeconds <~ fps gameFps
      keySignal = (,,,) <~ delta
                         ~ Keyboard.arrows
                         ~ Keyboard.space
                         ~ wsRecvData
  in sampleOn delta keySignal

-- ゲーム状態のシグナル
-- foldpでシグナルにする
gameStateSignal : Signal GameState
gameStateSignal = foldp stepGame initialGameState inputSignal

-- gameStateからsendDataを取り出すための関数
sendData gameState = gameState.sendData

-- サーバーへ送るデータ
-- WebSocketコネクションはJSでハンドリング
port wsSendData : Signal String
port wsSendData = let sendData = (\gameState -> gameState.sendData)
                      delta = inSeconds <~ fps requestFps
                  in dropRepeats <| sampleOn delta <| sendData <~ gameStateSignal

-- ディスプレイ関数
-- (ウィンドウサイズ) -> ゲームステート -> Form
display (windowWidth, windowHeight) gameState =
  let

      -- スクリーンタイルサイズ
      screenTileWidth = (/) tileWidth <| toFloat windowWidth
      screenTileHeight = (/) tileHeight <| toFloat windowHeight

      lastGameState = { gameState | screenTileWidth <- screenTileWidth
                                  , screenTileHeight <- screenTileHeight
                                  }

      marioImage = getImage lastGameState.mario (20, 35)
      marioPos = lastGameState.mario.pos

  in collage windowWidth windowHeight
    [move marioPos <| toForm marioImage]

-- エントリーポイント
main = display <~ Window.dimensions
                ~ gameStateSignal
