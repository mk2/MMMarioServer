module MMMarioRenderer where

import Graphics.Collage (..)

import MMMarioConfig (..)
import MMMarioType (..)

-- キャラクターイメージの取得
getImage chara (w, h) =
  image w h (concat [imageBaseUrl, chara.imageBaseName, "-", chara.imagePoseName, "-", chara.imageDireName, ".png"])

-- レンダー関数
render : (Int, Int) -> GameState -> Element
render (screenWidth, screenHeight) gameState =
    let
        -- スクリーンタイルサイズ
        screenTileWidth = screenWidth `div` tileWidth
        screenTileHeight = screenHeight `div` tileHeight

        lastGameState = { gameState | screenTileWidth <- screenTileWidth
                                    , screenTileHeight <- screenTileHeight
                                    }

        marioImage = getImage lastGameState.mario (20, 35)

        bgForm = filled bgColor <| rect (toFloat screenWidth) (toFloat screenHeight)

        marioForm = move lastGameState.mario.pos . toForm <| marioImage

        stageForm = renderStage (0, 0) []

        moveToX = toFloat <| -screenWidth `div` 2
        moveToY = toFloat <| -screenHeight `div` 2
        stageWholeForm = move (moveToX, moveToY) . group <| [stageForm, marioForm]

    in collage screenWidth screenHeight [bgForm, stageWholeForm]

renderStage : (Int, Int) -> [StageTile] -> Form
renderStage (stageTileWidth, stageTileHeight) stageTiles =
    if | (stageTileWidth * stageTileHeight) /= (length stageTiles) -> toForm . leftAligned . toText <| "no data"
       | otherwise -> toForm . centered . toText <| "ok data"