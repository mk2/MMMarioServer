module MMMarioType where

import MMMarioVector (..)

data RenderType = RChara | RBlock | RItem

data StageTile = None | Cloud | Ground

data StateName = Idle | Ongame | Postgame

data Result = Candidate | Winner | Loser

type UserInput = {
                   arr : {x : Int, y : Int}
                 , space : Bool
                 }

type Chara = {
               rect: Rect
             , spd : Vec
             , isTouchOnUpsideBlock : Bool
             , isTouchOnLeftBlock : Bool
             , isTouchOnDownBlock : Bool
             , isTouchOnRightBlock : Bool
             , mass : Float
             , imageBaseName : String
             , imagePoseName : String
             , imageDireName : String
             }

type GameState = {
                   self : Chara
                 , stateName : StateName
                 , ellapsedSeconds : Float
                 , blockGenInterval : Float
                 , blocks : [Rect]
                 , stageSize : Vec
                 , otherCharas : [Chara]
                 , sendData : String
                 , recvData : String
                 , clientName : String
                 , windowDims : (Int, Int)
                 , result : Result
                 , finish : Bool
                 }