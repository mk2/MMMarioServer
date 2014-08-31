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

{-| レクト同士の重なっている部分を解消する
 -}
resolveCollision : Rect -> Chara -> Chara
resolveCollision block m =
    let blkpos = block.origin
        wider = (getSizeW block) > (getSizeH block)
        ccen = getRectCenter m.rect
    in if | wider && isUpsidePos ccen blkpos -> { m | rect <- moveRect (0, -(getSizeH block)) m.rect
                                                    , isTouchOnUpsideBlock <- True }
          | wider && isDownPos ccen blkpos -> { m | rect <- moveRect (0, getSizeH block) m.rect
                                                  , isTouchOnDownBlock <- True }
          | isLeftPos ccen blkpos -> { m | rect <- moveRect (-(getSizeW block), 0) m.rect
                                         , isTouchOnLeftBlock <- True }
          | isRightPos ccen blkpos -> { m | rect <- moveRect (getSizeW block, 0) m.rect
                                          , isTouchOnRightBlock <- True }
          | otherwise -> m

{-| レクトとキャラの重なりをすべて解消する
    多分無限ループにはならない
 -}
resolveCollisions : [Rect] -> Chara -> Chara
resolveCollisions blocks m =
    let overlapRects = justs <| map (\r -> getOverlapRect r m.rect) blocks
        newM = foldl (\r mrect -> resolveCollision r m) m overlapRects
    in if | length overlapRects == 0 ->  newM
          | otherwise -> resolveCollisions overlapRects m

-- キャラクターの速度を計算
-- 計算方法
-- delta : t
calcCharaSpd : Float -> Vec -> Bool -> Chara -> Chara
calcCharaSpd delta moveStep willJump m =
    let -- ジャンプできるかどうか
        jumpable = willJump && m.isTouchOnDownBlock && not m.isTouchOnUpsideBlock
    in if   -- ジャンプ可能な場合
          | jumpable -> { m | spd <- addVec m.spd jumpStep }
            -- それ以外の場合
          | otherwise -> { m | spd <- addVec moveStep . addVec (multVec delta gravityStep) <| m.spd }

{-| キャラクターのいちを計算し、コリジョンを解消する
    コリジョンの解消前に接触フラグを全てFalseにしておく
 -}
calcCharaPos : GameState -> Float -> Chara -> Chara
calcCharaPos gameState delta m =
    let newPos = addVec m.rect.origin (multVec delta m.spd)
    in resolveCollisions gameState.blocks { m | rect <- {origin = newPos, size = m.rect.size}
                                              , isTouchOnUpsideBlock <- False
                                              , isTouchOnDownBlock <- False
                                              , isTouchOnRightBlock <- False
                                              , isTouchOnLeftBlock <- False }

-- キャラクターのイメージを更新
updateCharaImage m = m

-- ゲーム関数
-- (更新秒, (矢印キー上下, 矢印キー左右), キーボード, WS受信データ, クライアント名, ウィンドウサイズ, ブロック) -> ゲームステート -> ゲームステート
stepGame : (Float, {x : Int, y : Int}, Bool, String, String, (Int, Int), Rect) -> GameState -> GameState
stepGame (delta, arr, space, recvData, clientName, (winWidth, winHeight), blkRect) gameState =
    let
        -- ブロックの移動速度
        moveSpd = vec (0, 10)

        newBlks = filter checkBlock . map (\blkRect -> moveBlock delta blkRect 20000 moveSpd) <| gameState.blocks

        -- 移動速度
        moveStep = multVec moveCoeff (toFloat arr.x, 0)

        -- 新しい自キャラ位置の計算
        newSelf = log "self" <| updateCharaImage . calcCharaPos gameState delta . calcCharaSpd delta moveStep space <| gameState.self

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
                   , self <- newSelf
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
