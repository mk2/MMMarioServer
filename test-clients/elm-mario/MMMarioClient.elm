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
-- delta更新毎にkeySignal (デルタ秒, (矢印キー), スペースキー, WSからの受信データ, クライアント名, ウィンドウサイズ, (ランダムなブロックレクト)) を取得
inputSignal = let delta = inSeconds <~ fps gameFps
                  randomBlockX = Random.range 0 stageWidth delta
                  randomBlockY = Random.range 0 stageHeight delta
                  randomBlockWidth = Random.range minBlockSizeWidth maxBlockSizeWidth delta
                  randomBlockHeight = Random.range minBlockSizeHeight maxBlockSizeHeight delta
                  genBlock = \blkX blkY blkW blkH -> { origin = vec (blkX, blkY), size = vec (blkW, blkH) }
                  blockSignal = genBlock <~ randomBlockX ~ randomBlockY ~ randomBlockWidth ~ randomBlockHeight
                  keySignal = (,,,,,,) <~ delta
                                        ~ Keyboard.arrows
                                        ~ Keyboard.space
                                        ~ wsRecvData
                                        ~ clientName
                                        ~ Window.dimensions
                                        ~ blockSignal
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

moveBlock delta blkRect mass baseSpd =
    let massCoeff = (/) mass (getArea blkRect)
        moveStep = log "moveStep" <| multVec delta . multVec massCoeff <| baseSpd
    in moveRect moveStep blkRect

stageRect = { origin = zeroVec, size = vec (stageWidth, stageHeight) }

checkBlock blkRect =
    let overlapRect = getOverlapRect blkRect stageRect
    in case overlapRect of
        Nothing -> False
        _ -> True

resolveCollision : Rect -> Rect -> Rect
resolveCollision block crect =
    let blkpos = block.origin
        wider = (getSizeW block) > (getSizeH block)
        ccen = getRectCenter crect
    in if | wider && isUpsidePos ccen blkpos -> moveRect (0, -(getSizeH block)) crect
          | wider && isDownPos ccen blkpos -> moveRect (0, getSizeH block) crect
          | isLeftPos ccen blkpos -> moveRect (-(getSizeW block), 0) crect
          | isRightPos ccen blkpos -> moveRect (getSizeW block, 0) crect
          | otherwise -> crect

resolveCollisions : [Rect] -> Chara -> Chara
resolveCollisions blocks m =
    let overlapRects = justs <| map (\r -> getOverlapRect r m.rect) blocks
        newMrect = foldl (\r mrect -> resolveCollision r mrect) m.rect overlapRects
    in if | length overlapRects == 0 ->  { m | rect <- newMrect }
          | otherwise -> resolveCollisions overlapRects { m | rect <- newMrect }

-- キャラクターの速度を計算
-- 計算方法
-- delta : t
calcCharaSpd : Float -> Vec -> Vec -> Bool -> Chara -> Chara
calcCharaSpd delta moveStep gravityStep willJump m = m

-- キャラクターの位置を計算
-- 速度で位置を計算し、その位置が適切なものならばそれに更新、違っているならそのまま
calcCharaPos : GameState -> Float -> Chara -> Chara
calcCharaPos gameState delta m = m

-- キャラクターのイメージを更新
updateCharaImage m = m

-- ゲーム関数
-- (更新秒, (矢印キー上下, 矢印キー左右), キーボード, WS受信データ, クライアント名, ウィンドウサイズ, ブロック) -> ゲームステート -> ゲームステート
stepGame : (Float, {x : Int, y : Int}, Bool, String, String, (Int, Int), Rect) -> GameState -> GameState
stepGame (delta, arr, space, recvData, clientName, (winWidth, winHeight), blkRect) gameState =
    let
        d = log "blkRect" <| blkRect

        moveSpd = vec (0, 10)

        newBlks = filter checkBlock . map (\blkRect -> moveBlock delta blkRect 20000 moveSpd) <| gameState.blocks
        blks = log "blks" <| length newBlks

        -- 更新前の自キャラ
        --preSelf = gameState.self

        -- 移動速度
        moveStep = multVec moveCoeff (toFloat arr.x, 0)

        -- 新しい自キャラ位置の計算
        --newSelf = updateCharaImage . calcCharaPos gameState delta . calcCharaSpd delta moveStep gravityStep preSelf

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
                   , ellapsedSeconds <- gameState.ellapsedSeconds + delta
                   , otherCharas <- []
                   , clientName <- clientName
                   , blocks <- blkRect :: newBlks
                   , windowDims <- (winWidth, winHeight) }

-- ディスプレイ関数
-- (ウィンドウサイズ) -> ゲームステート -> Element
display windowSize gameState = R.render windowSize gameState

-- エントリーポイント
main = display <~ Window.dimensions
                ~ gameStateSignal
