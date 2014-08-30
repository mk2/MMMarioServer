module MMMarioClient where

{-| クライアントプログラム
@author mk2
 -}

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
import Random

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
-- delta更新毎にkeySignal (デルタ秒, (矢印キー), スペースキー, WSからの受信データ, クライアント名, ウィンドウサイズ, (ランダムなブロックサイズ)) を取得
inputSignal = let delta = inSeconds <~ fps gameFps
                  randomBlockWidth = Random.range minBlockSizeWidth maxBlockSizeWidth delta
                  randomBlockHeight = Random.range minBlockSizeHeight maxBlockSizeHeight delta
                  keySignal = (,,,,,,,) <~ delta
                                         ~ Keyboard.arrows
                                         ~ Keyboard.space
                                         ~ wsRecvData
                                         ~ clientName
                                         ~ Window.dimensions
                                         ~ randomBlockWidth
                                         ~ randomBlockHeight
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

-- キャラクターの速度を計算
-- 計算方法
-- delta : t
calcCharaSpd : Float -> Vec -> Vec -> Vec -> Bool -> Chara -> Chara
calcCharaSpd delta moveStep fricStep gravityStep willJump m =
    let
        -- ジャンプできるかどうか
        jumpable = willJump && m.isTouchOnDownBlock && not m.isTouchOnTopBlock

    in if   -- ジャンプキーが押されてかつ地面に触れていた場合ジャンプ可能
          | jumpable -> { m | spd <- marioJumpStep }

            -- 地面に触れていない場合、移動できない
          | not m.isTouchOnDownBlock -> { m | spd <- addVec fricStep gravityStep }

            -- それ以外の場合（全ての加速度が現在の加速度にたされる）
          | otherwise -> { m | spd <- addVec moveStep . addVec fricStep <| gravityStep }

-- キャラクターの位置を計算
-- 速度で位置を計算し、その位置が適切なものならばそれに更新、違っているならそのまま
calcCharaPos : GameState -> Float -> Chara -> Chara
calcCharaPos gameState delta m =
    let
        -- 新しい位置を計算
        (px, py) = addVec m.rect.origin <| multVec delta m.spd

    in m

-- キャラクターのイメージを更新
updateCharaImage m =
    m

-- ゲーム関数
-- (更新秒, (矢印キー上下, 矢印キー左右), キーボード, WS受信データ, クライアント名, ウィンドウサイズ, ランダム数) -> ゲームステート -> ゲームステート
stepGame : (Float, {x : Int, y : Int}, Bool, String, String, (Int, Int), Int, Int) -> GameState -> GameState
stepGame (delta, arr, space, recvData, clientName, (winWidth, winHeight), blkW, blkH) gameState =
    let
        d = delta
        -- 更新前のマリオ
        --preMario = gameState.mario

        -- 移動加速度
        --moveStep = multVec moveCoeff (toFloat arr.x, 0)

        -- 摩擦加速度
        --fricStep = multVec fricCoeff (negVec preMario.spd)

        -- 送信するマリオの位置情報
        {-- marioPosStr = "M" ++ (show . absRound . getx <| newMario.pos) ++ "," ++ (show . absRound . gety <| newMario.pos)

        -- 別キャラの位置
        poss = split "," recvData
        numCharas = (length poss) `div` 2
        maybeFloat = maybe 0.0 (\n -> n) . S.toFloat
        cnvToFloat = \[name, strX, strY] -> (name, (maybeFloat strX, maybeFloat strY))
        otherCharas = log "otherCharas" <| zip [1 .. numCharas] <| map cnvToFloat <| takeCycle 3 poss --}

    in { gameState |
                     sendData <- ""
                   , otherCharas <- []
                   , clientName <- clientName
                   , windowDims <- (winWidth, winHeight) }

-- ディスプレイ関数
-- (ウィンドウサイズ) -> ゲームステート -> Element
display windowSize gameState = R.render windowSize gameState

-- エントリーポイント
main = display <~ Window.dimensions
                ~ gameStateSignal
