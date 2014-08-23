module MMMarioType where

import MMMarioVector (..)

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
                 , otherCharas : [(String, Chara)]
                 , sendData : String
                 }