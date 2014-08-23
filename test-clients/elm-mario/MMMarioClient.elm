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
import MMMarioRenderer as R
import MMMarioUtil (..)
import MMMarioType (..)
import MMMarioVector (..)

{--================================================================--}
{-- Config --}
{--================================================================--}

-- ゲームのFPS
gameFps = 60

-- サーバーへの送信FPS
-- 上げるとすぐ死ぬので注意
requestFps = 0.2

-- リソースへのアクセスパス
resourceBaseUrl = "resources/"

imageBaseUrl = resourceBaseUrl ++ "images/"

-- 最大位置
maxPos = (10000, 10000)
minPos = (0, 0)

-- 最大速度
maxSpd = (10, 10)
minSpd = (-10, -10)

-- 標準のキャラクター
defaultChara = {
                 pos = zero
               , spd = zero
               , acc = zero
               , isTouchOnGround = False
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
initialGameState = {
                     mario = { defaultChara | pos <- (0, 100)
                                            , isTouchOnGround <- False
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
marioJumpAccel = (0, 2000)

-- 重力加速度
gravityAccel = (0, -900)

-- 摩擦係数
fricCoeff = 2

-- マリオ移動加速度係数
moveCoeff = 2000

-- タイルの幅(px)
tileWidth = 32

-- タイルの高さ(px)
tileHeight = 32

{--================================================================--}
{-- Signals --}
{--================================================================--}

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

{--================================================================--}
{-- 処理関数 --}
{--================================================================--}

-- キャラクターの加速度を計算
-- 計算方法
-- delta : t
-- moveAccel : 移動加速度
-- fricAccel : 摩擦加速度
-- gravityAccel : 重力加速度
-- willJump : ジャンプ状態に持っていくかどうか
calcCharaAccel delta moveAccel fricAccel gravityAccel willJump m =
  let
      -- ジャンプできるかどうか
      jumpable = willJump && m.isTouchOnGround

  in if
          -- ジャンプキーが押されてかつ地面に触れていた場合ジャンプ可能
        | jumpable -> { m | acc <- marioJumpAccel }

          -- 地面に触れていない場合、移動できない
        | not m.isTouchOnGround -> { m | acc <- addVec fricAccel gravityAccel }

          -- それ以外の場合（全ての加速度が現在の加速度にたされる）
        | otherwise -> { m | acc <- addVec moveAccel . addVec fricAccel <| gravityAccel }

-- キャラクターの位置を計算
-- mにある加速度で位置を計算し、その位置が適切なものならばそれに更新、違っているならそのまま
calcCharaPos delta m =
  let
      (sx, sy) = clampVec minSpd maxSpd . addVec m.spd . multVec delta <| m.acc
      (nx, ny) = addVec m.pos (sx, sy)
  in if | ny < 0 -> { m | pos <- clampVec minPos maxPos (nx, 0)
                        , spd <- (sx, 0)
                        , isTouchOnGround <- True }
        | otherwise -> { m | pos <- clampVec minPos maxPos (nx, ny)
                           , spd <- (sx, sy) }

-- キャラクターのイメージを更新
updateCharaImage m =
  m

-- キャラクターイメージの取得
getImage chara (w, h) =
  image w h (concat [imageBaseUrl, chara.imageBaseName, "-", chara.imagePoseName, "-", chara.imageDireName, ".png"])

-- ゲーム関数
-- (更新秒, (矢印キー上下, 矢印キー左右), キーボード) -> ゲームステート -> ゲームステート
stepGame : (Float, {x : Int, y : Int}, Bool, String) -> GameState -> GameState
stepGame (delta, arr, space, recvData) gameState =
  let
      -- 更新前のマリオ
      preMario = gameState.mario

      -- 移動加速度
      moveAccel = multVec moveCoeff (toFloat arr.x, 0)

      -- 摩擦加速度
      fricAccel = multVec fricCoeff (revVec preMario.spd)

      -- 更新関数
      updateChara = updateCharaImage . calcCharaPos delta . calcCharaAccel delta moveAccel fricAccel gravityAccel space

      -- マリオを更新する
      newMario = updateChara preMario

      -- 送信するマリオの位置情報
      marioPosStr = "M" ++ (show . absRound . getx <| newMario.pos) ++ "," ++ (show . absRound . gety <| newMario.pos)

      -- 別キャラの位置
      poss = split "," recvData
      numCharas = (length poss) `div` 2
      otherCharas = log "otherCharas" <| zip [1 .. numCharas] <| takeCycle 2 poss

  in { gameState | mario <- newMario
                 , sendData <- marioPosStr
     }

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
