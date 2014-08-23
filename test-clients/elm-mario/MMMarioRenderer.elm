module MMMarioRenderer where

import Graphics.Collage (..)
import Array (..)

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

        stageForm = renderStage (0, 0) gameState.stageTiles

        moveToX = toFloat <| -screenWidth `div` 2
        moveToY = toFloat <| -screenHeight `div` 2
        stageWholeForm = move (moveToX, moveToY) . group <| [stageForm, marioForm]

    in collage screenWidth screenHeight [bgForm, stageWholeForm]

renderStage : (Int, Int) -> [[StageTile]] -> Form
renderStage (stageTileWidth, stageTileHeight) stageTileRows =
    group . toList <| indexedMap (\row stageTileRow -> group . justs <| renderTileRow row stageTileRow) (fromList stageTileRows)

renderTileRow : Int -> [StageTile] -> [Maybe Form]
renderTileRow row stageTiles =
    let
        my = toFloat <| tileHeight * row
    in toList <| indexedMap (\mx stageTile -> renderTile ((toFloat <| tileWidth * mx), my) stageTile) (fromList stageTiles)

renderTile : (Float, Float) -> StageTile -> Maybe Form
renderTile moveXY stageTile =
    case stageTile of
        None -> Nothing
        Cloud -> Just (move moveXY . filled cldColor <| rect (toFloat tileWidth) (toFloat tileHeight))
        Ground -> Just (move moveXY . filled gndColor <| rect (toFloat tileWidth) (toFloat tileHeight))
        otherwise -> Nothing
