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
        -- 描画領域全体を移動するベクトル
        moveOriginVec = negVec . multVec 0.5 <| vec (screenWidth, screenHeight)

        -- クライアント名
        clientName = gameState.clientName

        -- 自キャラ
        selfForm = renderChara gameState.self

        -- 他のキャラ
        otherCharaForms = renderCharas gameState.otherCharas

        -- ブロック
        blkForms = group . map renderBlock <| gameState.blocks

        -- デバッグ用に受信データの表示を行う
        recvDataDispVec = vec (200, screenHeight - 20)
        recvDataForm = move recvDataDispVec . toForm . plainText <| "RECV: " ++ gameState.recvData

        -- デバッグ用に送信データの表示を行う
        sendDataDispVec = vec (200, screenHeight - 50)
        sendDataForm = move sendDataDispVec . toForm . plainText <| "SEND: " ++ gameState.sendData

    in collage screenWidth screenHeight <| [move moveOriginVec . group <| [blkForms, otherCharaForms, selfForm, recvDataForm, sendDataForm]]

renderCharas : [Chara] -> Form
renderCharas charas = group . map renderChara <| charas

renderChara : Chara -> Form
renderChara c =
    let img = getImage c (marioImageWidth, marioImageHeight)
        v = addVec c.rect.origin <| multVec 0.5 c.rect.size
    in move v . toForm <| img

renderBlock : Rect -> Form
renderBlock blkRect =
    let linSt = solid linColor
        w = getSizeW blkRect
        h = getSizeH blkRect
        v = addVec blkRect.origin <| multVec 0.5 blkRect.size
    in move v . outlined linSt <| rect w h