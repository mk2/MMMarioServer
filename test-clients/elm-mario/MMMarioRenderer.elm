module MMMarioRenderer where

import Graphics.Collage (..)
import Array (indexedMap, toList, fromList)

import MMMarioConfig (..)
import MMMarioType (..)

-- キャラクターイメージの取得
getImage chara (w, h) =
  image w h (concat [imageBaseUrl, chara.imageBaseName, "-", chara.imagePoseName, "-", chara.imageDireName, ".png"])

-- レンダー関数
render : (Int, Int) -> GameState -> Element
render (screenWidth, screenHeight) gameState =
    let
        -- クライアント名
        clientName = gameState.clientName

    in collage screenWidth screenHeight []
