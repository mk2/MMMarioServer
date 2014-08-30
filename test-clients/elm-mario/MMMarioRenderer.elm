module MMMarioRenderer where

import Graphics.Collage (..)
import Array (indexedMap, toList, fromList)
import Transform2D as T2D
import Debug (log)

import MMMarioVector (..)
import MMMarioConfig (..)
import MMMarioType (..)

-- キャラクターイメージの取得
getImage chara (w, h) =
  image w h (concat [imageBaseUrl, chara.imageBaseName, "-", chara.imagePoseName, "-", chara.imageDireName, ".png"])

-- レンダー関数
render : (Int, Int) -> GameState -> Element
render (screenWidth, screenHeight) gameState =
    let
        moveOriginVec = negVec . multVec 0.5 <| vec (screenWidth, screenHeight)

        -- クライアント名
        clientName = gameState.clientName

        blkForms = group . map renderBlock <| gameState.blocks

    in collage screenWidth screenHeight <| [move moveOriginVec . group <| [blkForms]]

renderBlock : Rect -> Form
renderBlock blkRect =
    let
        linSt = solid linColor
        w = getSizeW blkRect
        h = getSizeH blkRect
    in move blkRect.origin . outlined linSt <| rect w h