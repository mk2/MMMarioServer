import Graphics.Input (Input, input, button)
import Window
import Keyboard
import Graphics.Collage (..)
import Graphics.Element (image, fittedImage, croppedImage)
import Debug (log)

-- 自作ライブラリのimport
import Vector (..)

{--------------------------------------------------------------------}
{--
 設定値
 --}

-- ゲームのFPS
gameFps = 60

-- ゲームの初期状態
initialGameState = {
                     mario = {
                               pos = (0, 100)
                             , spd = (0, 0)
                             , acc = (0, 0)
                             , isTouchOnBlock = False
                             , isTouchOnGround = False
                             , mass = 100
                             , imageBaseName = "mario"
                             , imagePoseName = "stand"
                             , imageDireName = "right"
                             }
                   , stageTileWidth = 200
                   , stageTileHeight = 100
                   , screenTileWidth = 10
                   , screenTileHeight = 10
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
-- jump : ジャンプフラグ
calcCharaAccel delta moveAccel fricAccel gravityAccel jump m =
  let
      -- キャラクターのx,y座標
      x = fst m.pos
      y = snd m.pos

      -- 各加速度はdelta秒分の量にする
      dGravityAccel = multVec gravityAccel delta
      dMoveAccel = multVec moveAccel delta
      dFricAccel = multVec fricAccel delta
      dSmallFricAccel = multVec dFricAccel 0.2

      -- ジャンプできるかどうか
      jumpable = jump && m.isTouchOnGround

  in if | jumpable -> { m | acc <- addVec m.acc marioJumpAccel } -- ジャンプキーが押されてかつ地面に触れていた場合ジャンプ可能
        | not m.isTouchOnGround -> { m | acc <- addVec m.acc
                                             <| addVec dSmallFricAccel dGravityAccel } -- 地面に触れていない場合、移動できない
        | otherwise -> { m | acc <- addVec m.acc
                                 <| addVec dGravityAccel
                                 <| addVec dMoveAccel dFricAccel } -- それ以外の場合（全ての加速度が現在の加速度にたされる）

-- キャラクターの位置を計算
calcCharaPos delta m =
  let x = fst m.pos
      y = snd m.pos
      ax = fst m.acc
      ay = snd m.acc
      sx = ax * delta
      sy = ay * delta
  in if | m.isTouchOnBlock -> { m | acc <- zeroVec, spd <- zeroVec
                                  , isTouchOnGround <- True }
        | y < 0 -> { m | acc <- (ax, 0)
                       , pos <- (x, 0)
                       , isTouchOnGround <- True }
        | otherwise -> { m | pos <- addVec m.pos
                                      <| multVec m.spd delta
                           , spd <- (sx, sy)
                           , isTouchOnGround <- False }

-- キャラクターのイメージを更新
updateCharaImage m =
  m

-- キャラクターイメージの取得
getImage chara (w, h) =
  image w h (concat [chara.imageBaseName, "-", chara.imagePoseName, "-", chara.imageDireName, ".png"])

-- ゲーム関数
-- (更新秒, (矢印キー上下, 矢印キー左右), キーボード) -> ゲームステート -> ゲームステート
stepGame : (Float, {x : Int, y : Int}, Bool) -> GameState -> GameState
stepGame (delta, arr, space) gameState =
  let
      -- 更新前のマリオ
      preMario = gameState.mario

      -- 移動加速度
      moveAccel = multVec (toFloat arr.x, toFloat arr.y) moveCoeff

      -- 摩擦加速度
      fricAccel = multVec (revVec preMario.spd) fricCoeff

      -- 更新関数
      updateChara = updateCharaImage . calcCharaPos delta . calcCharaAccel delta moveAccel fricAccel gravityAccel space

      -- マリオを更新する
      newMario = log "new mario" <| updateChara <| preMario

  in { gameState | mario <- newMario }

-- 入力シグナル
-- delta更新毎にkeySignal (デルタ秒, (矢印キー), スペースキー) を取得
inputSignal =
  let delta = inSeconds <~ fps gameFps
      keySignal = (,,) <~ delta
                        ~ Keyboard.arrows
                        ~ Keyboard.space
  in sampleOn delta keySignal

-- ディスプレイ関数
-- (ウィンドウサイズ) -> ゲームステート -> Form
display (windowWidth, windowHeight) gameState  =
  let

      -- スクリーンタイルサイズ
      screenTileWidth = (/) tileWidth <| toFloat windowWidth
      screenTileHeight = (/) tileHeight <| toFloat windowHeight


      lastGameState = { gameState | screenTileWidth <- screenTileWidth
                                  , screenTileHeight <- screenTileHeight }

      marioImage = getImage lastGameState.mario (20, 35)
      marioPos = log "mario pos" lastGameState.mario.pos

  in collage windowWidth windowHeight
    [move marioPos <| toForm marioImage]

-- エントリーポイント
main = display <~ Window.dimensions
                ~ foldp stepGame initialGameState inputSignal
