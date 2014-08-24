module MMMarioClient where

{--
@author mk2
@description
 --}

import Graphics.Input (Input, input, button)
import Window
import Keyboard
import Graphics.Collage (..)
import Graphics.Element (image, fittedImage, croppedImage)
import WebSocket
import Debug (log)
import String (split, show)
import Maybe
import String as S

-- 自作ライブラリのimport
import MMMarioVector (..)
import MMMarioUtil (..)
import MMMarioType (..)
import MMMarioRenderer as R
import MMMarioConfig (..)

{--================================================================--}
{-- Signals --}
{--================================================================--}

-- WebSocketからの受信データ
port wsRecvData : Signal String

-- クライアント名、JS側から与える
port clientName : Signal String

-- 入力シグナル
-- delta更新毎にkeySignal (デルタ秒, (矢印キー), スペースキー, WSからの受信データ) を取得
inputSignal =
  let delta = inSeconds <~ fps gameFps
      keySignal = (,,,,) <~ delta
                         ~ Keyboard.arrows
                         ~ Keyboard.space
                         ~ wsRecvData
                         ~ clientName
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
      jumpable = willJump && m.isTouchOnDownBlock

  in if
          -- ジャンプキーが押されてかつ地面に触れていた場合ジャンプ可能
        | jumpable -> { m | acc <- marioJumpAccel }

          -- 地面に触れていない場合、移動できない
        | not m.isTouchOnDownBlock -> { m | acc <- addVec fricAccel gravityAccel }

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
                        , isTouchOnDownBlock <- True }
        | otherwise -> { m | pos <- clampVec minPos maxPos (nx, ny)
                           , spd <- (sx, sy) }

-- キャラクターのイメージを更新
updateCharaImage m =
  m

-- ゲーム関数
-- (更新秒, (矢印キー上下, 矢印キー左右), キーボード, WS受信データ, クライアント名) -> ゲームステート -> ゲームステート
stepGame : (Float, {x : Int, y : Int}, Bool, String, String) -> GameState -> GameState
stepGame (delta, arr, space, recvData, clientName) gameState =
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
      maybeFloat = maybe 0.0 (\n -> n) . S.toFloat
      cnvToFloat = \[name, strX, strY] -> (name, (maybeFloat strX, maybeFloat strY))
      otherCharas = log "otherCharas" <| zip [1 .. numCharas] <| map cnvToFloat <| takeCycleAsList 3 poss

  in { gameState | mario <- newMario
                 , sendData <- marioPosStr
                 , otherCharas <- otherCharas
                 , clientName <- clientName }

-- ディスプレイ関数
-- (ウィンドウサイズ) -> ゲームステート -> Element
display windowSize gameState = R.render windowSize gameState

-- エントリーポイント
main = display <~ Window.dimensions
                ~ gameStateSignal
