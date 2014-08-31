module MMMarioType where

import MMMarioVector (..)

data RenderType = RChara | RBlock | RItem

data StageTile = None | Cloud | Ground

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
                 , ellapsedSeconds : Float
                 , blocks : [Rect]
                 , stageSize : Vec
                 , otherCharas : [(Int, (String, Vec))]
                 , sendData : String
                 , clientName : String
                 , windowDims : (Int, Int)
                 }