module MMMarioType where

import MMMarioVector (..)

data RenderType = RChara | RBlock | RItem

data StageTile = None | Cloud | Ground

type UserInput = {
                   arr : {x : Int, y : Int}
                 , space : Bool
                 }

type Chara = {
               pos : Vec
             , spd : Vec
             , acc : Vec
             , isTouchOnTopBlock : Bool
             , isTouchOnLeftBlock : Bool
             , isTouchOnDownBlock : Bool
             , isTouchOnRightBlock : Bool
             , mass : Float
             , imageBaseName : String
             , imagePoseName : String
             , imageDireName : String
             }

type GameState = {
                   mario : Chara
                 , stageTileWidth : Float
                 , stageTileHeight : Float
                 , stageTiles : [[StageTile]]
                 , screenTileWidth : Float
                 , screenTileHeight : Float
                 , otherCharas : [(Int, (String, Vec))]
                 , sendData : String
                 , clientName : String
                 }