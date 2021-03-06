import Graphics.Input (Input, input, button)
import Graphics.Input.Field (Content, noContent, field, defaultStyle)
import Dict
import Text (centered, toText)
import WebSocket
import Debug (log)

{--
    ElmでWebsocket
    @author mk2
 --}

-- リクエストURL
-- url = "ws://echo.websocket.org"
url = "ws://localhost:8081"

{--------------
ユーザー入力周り
 --------------}

-- サンプルテキスト入力フィールド
msg : Input Content
msg = input noContent

-- 送信ボタンインプット
send : Input String
send = input ""

-- メッセージ入力フォーム
msgField : Content -> Element
msgField = field defaultStyle msg.handle id "Msg"

-- 送信ボタンを表示するための関数
sendButton : Content -> Element
sendButton msgContent =
    button send.handle msgContent.string "Send"


-- レスポンスを表示する関数
dispResponse : String -> Element
dispResponse response =
    let toElem = centered . toText
    in toElem <| "" ++ response

contentString content = content.string

-- レスポンスを示すシグナル
responseSignal : Signal String
responseSignal = WebSocket.connect url <| sampleOn send.signal <| contentString <~ msg.signal

-- 入力フォームを表示するための関数
msgForm : Content -> Element
msgForm msgContent =
    flow right [
                 msgField msgContent
               , sendButton msgContent
               ]

-- 全体（入力フォーム、送信ボタン）を表示するための関数
display msgContent response =
    flow down [
                msgForm msgContent
              , dispResponse response
              ]

main = display <~ msg.signal ~ responseSignal