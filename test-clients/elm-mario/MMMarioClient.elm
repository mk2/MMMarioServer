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
import Char

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
                  fIsDown = Keyboard.isDown . Char.toCode <| 'F'
                  genKeys = \arrows space shift keyF -> (arrows, space, shift, keyF)
                  keysSignal = genKeys <~ Keyboard.arrows ~ Keyboard.space ~ Keyboard.shift ~ fIsDown
                  keySignal = (,,,,,) <~ delta
                                       ~ keysSignal
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
        moveStep = multVec delta . multVec massCoeff <| baseSpd
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
                                                    , isTouchOnUpsideBlock <- True
                                                    , spd <- multVec 0.5 . negyVec <| m.spd }
          | wider && isDownPos ccen blkpos -> { m | rect <- moveRect (0, getSizeH block) m.rect
                                                  , isTouchOnDownBlock <- True
                                                  , spd <- zeroyVec m.spd }
          | isLeftPos ccen blkpos -> { m | rect <- moveRect (getSizeW block, 0) m.rect
                                         , isTouchOnLeftBlock <- True
                                         , spd <- negxVec m.spd }
          | isRightPos ccen blkpos -> { m | rect <- moveRect (-(getSizeW block), 0) m.rect
                                          , isTouchOnRightBlock <- True
                                          , spd <- negxVec m.spd }
          | otherwise -> m

{-| レクトとキャラの重なりをすべて解消する
 -}
resolveCollisions : [Rect] -> Chara -> Chara
resolveCollisions blocks m =
    let overlapRects = log "rect" <| justs <| map (\r -> getOverlapRect r m.rect) blocks
        newM = foldl (\r m -> resolveCollision r m) m <| overlapRects
    in { newM | rect <- clampRect minPos maxPos newM.rect }

-- キャラクターの速度を計算
-- 計算方法
-- delta : t
calcCharaSpd : Float -> Vec -> Bool -> Bool -> Chara -> Chara
calcCharaSpd delta moveStep speedUp willJump m =
    let -- ジャンプできるかどうか
        jumpable = willJump && m.isTouchOnDownBlock && not m.isTouchOnUpsideBlock
    in if   -- 加速状態かつジャンプ可能な場合
          | speedUp && jumpable -> { m | spd <- addVec m.spd . multVec 2.0 <| jumpStep }
            -- 加速状態かつジャンプ以外
          | speedUp && not jumpable -> { m | spd <- addVec (multVec 2.0 moveStep) . addVec (multVec delta gravityStep) <| m.spd  }
            -- 加速状態ではなくジャンプ可能な場合
          | not speedUp && jumpable -> { m | spd <- addVec m.spd jumpStep }
            -- 加速状態ではなくジャンプ以外
          | not speedUp && not jumpable -> { m | spd <- addVec moveStep . addVec (multVec delta gravityStep) <| m.spd }
            -- それ以外
          | otherwise -> m

{-| キャラクターのいちを計算し、コリジョンを解消する
    コリジョンの解消前に接触フラグを全てFalseにしておく
 -}
calcCharaPos : [Rect] -> Float -> Chara -> Chara
calcCharaPos blocks delta m =
    let newPos = addVec m.rect.origin (multVec delta m.spd)
    in resolveCollisions blocks { m | rect <- {origin = newPos, size = m.rect.size}
                                      , isTouchOnUpsideBlock <- False
                                      , isTouchOnDownBlock <- False
                                      , isTouchOnRightBlock <- False
                                      , isTouchOnLeftBlock <- False
                                      , spd <- (0.0, gety m.spd) }

-- キャラクターのイメージを更新
updateCharaImage m = m

-- キャラクターの生死を判定
checkCharaLiving chara = False

-- ゲーム関数
-- (更新秒, (矢印キー上下, 矢印キー左右), キーボード, WS受信データ, クライアント名, ウィンドウサイズ, ブロック) -> ゲームステート -> ゲームステート
stepGame (delta, (arr, space, shift, keyF), recvData, clientName, (winWidth, winHeight), blkRect) gameState =
    let
        -- ゲームのステート
        stateName = gameState.stateName

        -- ブロックの移動速度
        moveSpd = vec (0, 10)

        -- 新規ブロック
        newBlks = filter checkBlock . map (\blkRect -> moveBlock delta blkRect 20000 moveSpd) <| gameState.blocks

        -- 新規ブロック文字列
        sendGenBlkData = "BLK " ++ rectToString blkRect

        -- 新規ブロック生成フラグ
        isGenBlk = gameState.blockGenInterval > 2.0

        -- 移動速度
        moveStep = multVec moveCoeff (toFloat arr.x, 0)

        -- 新しい自キャラ位置の計算
        newSelf = updateCharaImage . calcCharaPos newBlks delta . calcCharaSpd delta moveStep shift space <| gameState.self

        -- キャラが死んだかどうか
        isDie = checkCharaLiving newSelf

        -- 送信するマリオの位置情報
        sendCharaRectData = convCharaToSend newSelf

        -- コマンド
        command = S.slice 0 3 recvData

    in if
            -- Idle 状態
          | stateName == Idle && command == "RED" -> { gameState | stateName <- Ongame
                                                                 , self      <- newSelf
                                                                 , ellapsedSeconds <- gameState.ellapsedSeconds + delta
                                                                 , blockGenInterval <- gameState.blockGenInterval + delta
                                                                 , sendData  <- "" }

          | stateName == Idle                     -> { gameState | stateName       <- Idle
                                                                 , self            <- newSelf
                                                                 , ellapsedSeconds <- gameState.ellapsedSeconds + delta
                                                                 , blockGenInterval <- gameState.blockGenInterval + delta
                                                                 , sendData        <- "" }

            -- Ongame 状態
          | stateName == Ongame && command == "REC" -> { gameState | stateName <- Ongame
                                                                   , self <- newSelf
                                                                   , ellapsedSeconds <- gameState.ellapsedSeconds + delta
                                                                   , blockGenInterval <- gameState.blockGenInterval + delta
                                                                   , blocks <- newBlks
                                                                   , otherCharas <- convRecvToCharas recvData
                                                                   , sendData <- sendCharaRectData }

          | stateName == Ongame && command == "BLK" -> { gameState | stateName <- Ongame
                                                                   , self <- newSelf
                                                                   , ellapsedSeconds <- gameState.ellapsedSeconds + delta
                                                                   , blockGenInterval <- gameState.blockGenInterval + delta
                                                                   , blocks <- (convRecvToBlock recvData) :: newBlks
                                                                   , sendData <- sendCharaRectData }

          | stateName == Ongame && command == "WIN" -> { gameState | stateName <- Postgame
                                                                   , ellapsedSeconds <- gameState.ellapsedSeconds + delta
                                                                   , blockGenInterval <- gameState.blockGenInterval + delta
                                                                   , blocks <- newBlks
                                                                   , result <- Winner
                                                                   , finish <- True }

          | stateName == Ongame && isDie            -> { gameState | stateName <- Postgame
                                                                   , ellapsedSeconds <- gameState.ellapsedSeconds + delta
                                                                   , blockGenInterval <- gameState.blockGenInterval + delta
                                                                   , blocks <- newBlks
                                                                   , result <- Loser }

          | stateName == Ongame && command == "REC" && isGenBlk
                                                    -> { gameState | stateName <- Ongame
                                                                   , self <- newSelf
                                                                   , ellapsedSeconds <- gameState.ellapsedSeconds + delta
                                                                   , blockGenInterval <- gameState.blockGenInterval + delta
                                                                   , blocks <- blkRect :: newBlks
                                                                   , otherCharas <- convRecvToCharas recvData
                                                                   , sendData <- sendCharaRectData ++ "|" ++ sendGenBlkData }

          | stateName == Ongame && command == "BLK" && isGenBlk
                                                    -> { gameState | stateName <- Ongame
                                                                   , self <- newSelf
                                                                   , ellapsedSeconds <- gameState.ellapsedSeconds + delta
                                                                   , blockGenInterval <- gameState.blockGenInterval + delta
                                                                   , blocks <- blkRect :: (convRecvToBlock recvData) :: newBlks
                                                                   , otherCharas <- convRecvToCharas recvData
                                                                   , sendData <- sendCharaRectData ++ "|" ++ sendGenBlkData }

          | stateName == Ongame && isGenBlk         -> { gameState | stateName <- Ongame
                                                                   , self <- newSelf
                                                                   , ellapsedSeconds <- gameState.ellapsedSeconds + delta
                                                                   , blockGenInterval <- gameState.blockGenInterval + delta
                                                                   , blocks <- blkRect :: newBlks
                                                                   , sendData <- sendCharaRectData ++ "|" ++ sendGenBlkData }

          | stateName == Ongame                     -> { gameState | stateName <- Ongame
                                                                   , self <- newSelf
                                                                   , ellapsedSeconds <- gameState.ellapsedSeconds + delta
                                                                   , blockGenInterval <- gameState.blockGenInterval + delta
                                                                   , sendData <- sendCharaRectData }

            -- Postgame 状態
          | stateName == Postgame && command == "LOS" -> { gameState | stateName <- Postgame
                                                                     , result    <- Loser
                                                                     , finish    <- True }

            -- デフォルト
          | otherwise ->  gameState

-- ディスプレイ関数
-- (ウィンドウサイズ) -> ゲームステート -> Element
display windowSize gameState = R.render windowSize gameState

-- エントリーポイント
main = display <~ Window.dimensions
                ~ gameStateSignal
