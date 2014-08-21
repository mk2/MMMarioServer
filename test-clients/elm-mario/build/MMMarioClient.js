Elm.MMMarioClient = Elm.MMMarioClient || {};
Elm.MMMarioClient.make = function (_elm) {
   "use strict";
   _elm.MMMarioClient = _elm.MMMarioClient || {};
   if (_elm.MMMarioClient.values)
   return _elm.MMMarioClient.values;
   var _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   _A = _N.Array.make(_elm),
   _E = _N.Error.make(_elm),
   $moduleName = "MMMarioClient";
   var Basics = Elm.Basics.make(_elm);
   var Color = Elm.Color.make(_elm);
   var Debug = Elm.Debug.make(_elm);
   var Graphics = Graphics || {};
   Graphics.Collage = Elm.Graphics.Collage.make(_elm);
   var Graphics = Graphics || {};
   Graphics.Element = Elm.Graphics.Element.make(_elm);
   var Graphics = Graphics || {};
   Graphics.Input = Elm.Graphics.Input.make(_elm);
   var Keyboard = Elm.Keyboard.make(_elm);
   var List = Elm.List.make(_elm);
   var Maybe = Elm.Maybe.make(_elm);
   var Native = Native || {};
   Native.Json = Elm.Native.Json.make(_elm);
   var Native = Native || {};
   Native.Ports = Elm.Native.Ports.make(_elm);
   var Signal = Elm.Signal.make(_elm);
   var String = Elm.String.make(_elm);
   var Text = Elm.Text.make(_elm);
   var Time = Elm.Time.make(_elm);
   var Vector = Elm.Vector.make(_elm);
   var WebSocket = Elm.WebSocket.make(_elm);
   var Window = Elm.Window.make(_elm);
   var _op = {};
   var sendData = function (gameState) {
      return gameState.sendData;
   };
   var wsRecvData = Native.Ports.portIn("wsRecvData",
   Native.Ports.incomingSignal(function (v) {
      return typeof v === "string" || typeof v === "object" && v instanceof String ? v : _E.raise("invalid input, expecting JSString but got " + v);
   }));
   var list2tuple = function (l) {
      return {ctor: "_Tuple2"
             ,_0: List.head(l)
             ,_1: List.last(l)};
   };
   var takeCycle$ = F3(function (n,
   l,
   accum) {
      return function () {
         var notEnough = _U.cmp(List.length(l),
         n) < 0;
         return notEnough ? accum : A3(takeCycle$,
         n,
         A2(List.drop,n,l),
         _L.append(accum,
         _L.fromArray([list2tuple(A2(List.take,
         n,
         l))])));
      }();
   });
   var takeCycle = F2(function (n,
   l) {
      return A3(takeCycle$,
      n,
      l,
      _L.fromArray([]));
   });
   var updateCharaImage = function (m) {
      return m;
   };
   var UserInput = F2(function (a,
   b) {
      return {_: {}
             ,arr: a
             ,space: b};
   });
   var GameState = F7(function (a,
   b,
   c,
   d,
   e,
   f,
   g) {
      return {_: {}
             ,mario: a
             ,otherCharas: f
             ,screenTileHeight: e
             ,screenTileWidth: d
             ,sendData: g
             ,stageTileHeight: c
             ,stageTileWidth: b};
   });
   var Chara = F9(function (a,
   b,
   c,
   d,
   e,
   f,
   g,
   h,
   i) {
      return {_: {}
             ,acc: c
             ,imageBaseName: g
             ,imageDireName: i
             ,imagePoseName: h
             ,isTouchOnBlock: d
             ,isTouchOnGround: e
             ,mass: f
             ,pos: a
             ,spd: b};
   });
   var RItem = {ctor: "RItem"};
   var RBlock = {ctor: "RBlock"};
   var RChara = {ctor: "RChara"};
   var tileHeight = 32;
   var tileWidth = 32;
   var moveCoeff = 200000;
   var fricCoeff = 500;
   var gravityAccel = {ctor: "_Tuple2"
                      ,_0: 0
                      ,_1: -10000};
   var marioJumpAccel = {ctor: "_Tuple2"
                        ,_0: 0
                        ,_1: 10000};
   var calcCharaAccel = F6(function (delta,
   moveAccel,
   fricAccel,
   gravityAccel,
   willJump,
   m) {
      return function () {
         var jumpable = willJump && m.isTouchOnGround;
         var dFricAccel = A2(Vector.multVec,
         fricAccel,
         delta);
         var dSmallFricAccel = A2(Vector.multVec,
         dFricAccel,
         0.2);
         var dMoveAccel = Debug.log("dMoveAccel")(A2(Vector.multVec,
         moveAccel,
         delta));
         var dGravityAccel = A2(Vector.multVec,
         gravityAccel,
         delta);
         var y = Vector.gety(m.pos);
         var x = Vector.getx(m.pos);
         return jumpable ? _U.replace([["acc"
                                       ,A2(Vector.addVec,
                                       m.acc,
                                       marioJumpAccel)]],
         m) : Basics.not(m.isTouchOnGround) ? _U.replace([["acc"
                                                          ,Vector.addVec(m.acc)(A2(Vector.addVec,
                                                          dSmallFricAccel,
                                                          dGravityAccel))]],
         m) : _U.replace([["acc"
                          ,Debug.log("accel")(Vector.addVec(m.acc)(Vector.addVec(dGravityAccel)(A2(Vector.addVec,
                          dMoveAccel,
                          dFricAccel))))]],
         m);
      }();
   });
   var defaultChara = {_: {}
                      ,acc: Vector.zero
                      ,imageBaseName: ""
                      ,imageDireName: ""
                      ,imagePoseName: ""
                      ,isTouchOnBlock: false
                      ,isTouchOnGround: false
                      ,mass: 100
                      ,pos: Vector.zero
                      ,spd: Vector.zero};
   var initialGameState = {_: {}
                          ,mario: _U.replace([["pos"
                                              ,{ctor: "_Tuple2"
                                               ,_0: 0
                                               ,_1: 100}]
                                             ,["imageBaseName","mario"]
                                             ,["imagePoseName","stand"]
                                             ,["imageDireName","right"]],
                          defaultChara)
                          ,otherCharas: _L.fromArray([])
                          ,screenTileHeight: 10
                          ,screenTileWidth: 10
                          ,sendData: ""
                          ,stageTileHeight: 100
                          ,stageTileWidth: 200};
   var minPos = {ctor: "_Tuple2"
                ,_0: 0
                ,_1: 0};
   var maxPos = {ctor: "_Tuple2"
                ,_0: 1000
                ,_1: 1000};
   var calcCharaPos = F2(function (delta,
   m) {
      return function () {
         var ay = Debug.log("accely")(Vector.gety(m.acc));
         var sy = ay * delta;
         var ax = Debug.log("accelx")(Vector.getx(m.acc));
         var sx = ax * delta;
         var y = Vector.gety(m.pos);
         var x = Vector.getx(m.pos);
         return m.isTouchOnBlock ? _U.replace([["acc"
                                               ,Vector.zero]
                                              ,["spd",Vector.zero]
                                              ,["isTouchOnGround",true]],
         m) : _U.cmp(y,
         0) < 0 ? _U.replace([["acc"
                              ,{ctor: "_Tuple2",_0: ax,_1: 0}]
                             ,["pos"
                              ,A3(Vector.clampVec,
                              minPos,
                              maxPos,
                              {ctor: "_Tuple2",_0: x,_1: 0})]
                             ,["isTouchOnGround",true]],
         m) : _U.replace([["pos"
                          ,A2(Vector.clampVec,
                          minPos,
                          maxPos)(Vector.addVec(m.pos)(A2(Vector.multVec,
                          m.spd,
                          delta)))]
                         ,["spd"
                          ,{ctor: "_Tuple2"
                           ,_0: sx
                           ,_1: sy}]
                         ,["isTouchOnGround",false]],
         m);
      }();
   });
   var stepGame = F2(function (_v0,
   gameState) {
      return function () {
         switch (_v0.ctor)
         {case "_Tuple4":
            return function () {
                 var poss = A2(String.split,
                 ",",
                 _v0._3);
                 var numCharas = List.length(poss) / 2 | 0;
                 var otherCharas = Debug.log("otherCharas")(List.zip(_L.range(1,
                 numCharas))(A2(takeCycle,
                 2,
                 poss)));
                 var absRound = function ($) {
                    return Basics.round(Basics.abs($));
                 };
                 var moveAccel = Debug.log("moveAccel")(A2(Vector.multVec,
                 {ctor: "_Tuple2"
                 ,_0: Basics.toFloat(_v0._1.x)
                 ,_1: Basics.toFloat(_v0._1.y)},
                 moveCoeff));
                 var preMario = gameState.mario;
                 var fricAccel = A2(Vector.multVec,
                 Vector.revVec(preMario.spd),
                 fricCoeff);
                 var updateChara = function ($) {
                    return updateCharaImage(calcCharaPos(_v0._0)(A5(calcCharaAccel,
                    _v0._0,
                    moveAccel,
                    fricAccel,
                    gravityAccel,
                    _v0._2)($)));
                 };
                 var newMario = updateChara(preMario);
                 var marioPosStr = _L.append("M",
                 _L.append(String.show(absRound(Vector.getx(newMario.pos))),
                 _L.append(",",
                 String.show(absRound(Vector.gety(newMario.pos))))));
                 return _U.replace([["mario"
                                    ,newMario]
                                   ,["sendData",marioPosStr]],
                 gameState);
              }();}
         _E.Case($moduleName,
         "between lines 184 and 211");
      }();
   });
   var resourceBaseUrl = "resources/";
   var imageBaseUrl = _L.append(resourceBaseUrl,
   "images/");
   var getImage = F2(function (chara,
   _v6) {
      return function () {
         switch (_v6.ctor)
         {case "_Tuple2":
            return A3(Graphics.Element.image,
              _v6._0,
              _v6._1,
              List.concat(_L.fromArray([imageBaseUrl
                                       ,chara.imageBaseName
                                       ,"-"
                                       ,chara.imagePoseName
                                       ,"-"
                                       ,chara.imageDireName
                                       ,".png"])));}
         _E.Case($moduleName,
         "on line 167, column 3 to 116");
      }();
   });
   var display = F2(function (_v10,
   gameState) {
      return function () {
         switch (_v10.ctor)
         {case "_Tuple2":
            return function () {
                 var screenTileHeight = F2(function (x,
                 y) {
                    return x / y;
                 })(tileHeight)(Basics.toFloat(_v10._1));
                 var screenTileWidth = F2(function (x,
                 y) {
                    return x / y;
                 })(tileWidth)(Basics.toFloat(_v10._0));
                 var lastGameState = _U.replace([["screenTileWidth"
                                                 ,screenTileWidth]
                                                ,["screenTileHeight"
                                                 ,screenTileHeight]],
                 gameState);
                 var marioImage = A2(getImage,
                 lastGameState.mario,
                 {ctor: "_Tuple2"
                 ,_0: 20
                 ,_1: 35});
                 var marioPos = lastGameState.mario.pos;
                 return A3(Graphics.Collage.collage,
                 _v10._0,
                 _v10._1,
                 _L.fromArray([Graphics.Collage.move(marioPos)(Graphics.Collage.toForm(marioImage))]));
              }();}
         _E.Case($moduleName,
         "between lines 244 and 258");
      }();
   });
   var requestFps = 30;
   var gameFps = 60;
   var inputSignal = function () {
      var delta = A2(Signal._op["<~"],
      Time.inSeconds,
      Time.fps(gameFps));
      var keySignal = A2(Signal._op["~"],
      A2(Signal._op["~"],
      A2(Signal._op["~"],
      A2(Signal._op["<~"],
      F4(function (v0,v1,v2,v3) {
         return {ctor: "_Tuple4"
                ,_0: v0
                ,_1: v1
                ,_2: v2
                ,_3: v3};
      }),
      delta),
      Keyboard.arrows),
      Keyboard.space),
      wsRecvData);
      return A2(Signal.sampleOn,
      delta,
      keySignal);
   }();
   var gameStateSignal = A3(Signal.foldp,
   stepGame,
   initialGameState,
   inputSignal);
   var wsSendData = Native.Ports.portOut("wsSendData",
   Native.Ports.outgoingSignal(function (v) {
      return v;
   }),
   function () {
      var delta = A2(Signal._op["<~"],
      Time.inSeconds,
      Time.fps(requestFps));
      var sendData = function (gameState) {
         return gameState.sendData;
      };
      return Signal.dropRepeats(Signal.sampleOn(delta)(A2(Signal._op["<~"],
      sendData,
      gameStateSignal)));
   }());
   var main = A2(Signal._op["~"],
   A2(Signal._op["<~"],
   display,
   Window.dimensions),
   gameStateSignal);
   _elm.MMMarioClient.values = {_op: _op
                               ,gameFps: gameFps
                               ,requestFps: requestFps
                               ,resourceBaseUrl: resourceBaseUrl
                               ,imageBaseUrl: imageBaseUrl
                               ,maxPos: maxPos
                               ,minPos: minPos
                               ,defaultChara: defaultChara
                               ,initialGameState: initialGameState
                               ,marioJumpAccel: marioJumpAccel
                               ,gravityAccel: gravityAccel
                               ,fricCoeff: fricCoeff
                               ,moveCoeff: moveCoeff
                               ,tileWidth: tileWidth
                               ,tileHeight: tileHeight
                               ,calcCharaAccel: calcCharaAccel
                               ,calcCharaPos: calcCharaPos
                               ,updateCharaImage: updateCharaImage
                               ,getImage: getImage
                               ,list2tuple: list2tuple
                               ,takeCycle: takeCycle
                               ,takeCycle$: takeCycle$
                               ,stepGame: stepGame
                               ,inputSignal: inputSignal
                               ,gameStateSignal: gameStateSignal
                               ,sendData: sendData
                               ,display: display
                               ,main: main
                               ,RChara: RChara
                               ,RBlock: RBlock
                               ,RItem: RItem
                               ,Chara: Chara
                               ,GameState: GameState
                               ,UserInput: UserInput};
   return _elm.MMMarioClient.values;
};Elm.Vector = Elm.Vector || {};
Elm.Vector.make = function (_elm) {
   "use strict";
   _elm.Vector = _elm.Vector || {};
   if (_elm.Vector.values)
   return _elm.Vector.values;
   var _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   _A = _N.Array.make(_elm),
   _E = _N.Error.make(_elm),
   $moduleName = "Vector";
   var Basics = Elm.Basics.make(_elm);
   var Color = Elm.Color.make(_elm);
   var Graphics = Graphics || {};
   Graphics.Collage = Elm.Graphics.Collage.make(_elm);
   var Graphics = Graphics || {};
   Graphics.Element = Elm.Graphics.Element.make(_elm);
   var List = Elm.List.make(_elm);
   var Maybe = Elm.Maybe.make(_elm);
   var Native = Native || {};
   Native.Json = Elm.Native.Json.make(_elm);
   var Native = Native || {};
   Native.Ports = Elm.Native.Ports.make(_elm);
   var Signal = Elm.Signal.make(_elm);
   var String = Elm.String.make(_elm);
   var Text = Elm.Text.make(_elm);
   var Time = Elm.Time.make(_elm);
   var _op = {};
   var gety = Basics.snd;
   var getx = Basics.fst;
   var clampVec = F3(function (minVec,
   maxVec,
   vec) {
      return function () {
         var minY = gety(minVec);
         var minX = getx(minVec);
         var maxY = gety(maxVec);
         var maxX = getx(maxVec);
         var y = gety(vec);
         var x = getx(vec);
         return {ctor: "_Tuple2"
                ,_0: A3(Basics.clamp,
                minX,
                maxX,
                x)
                ,_1: A3(Basics.clamp,
                minY,
                maxY,
                y)};
      }();
   });
   var unit = {ctor: "_Tuple2"
              ,_0: 1
              ,_1: 1};
   var revVec = function (_v0) {
      return function () {
         switch (_v0.ctor)
         {case "_Tuple2":
            return {ctor: "_Tuple2"
                   ,_0: 0 - _v0._0
                   ,_1: 0 - _v0._1};}
         _E.Case($moduleName,
         "on line 32, column 4 to 10");
      }();
   };
   var dotVec = F2(function (_v4,
   _v5) {
      return function () {
         switch (_v5.ctor)
         {case "_Tuple2":
            return function () {
                 switch (_v4.ctor)
                 {case "_Tuple2":
                    return _v4._0 * _v5._1 + _v4._1 * _v5._0;}
                 _E.Case($moduleName,
                 "on line 27, column 3 to 18");
              }();}
         _E.Case($moduleName,
         "on line 27, column 3 to 18");
      }();
   });
   var multVec = F2(function (_v12,
   k) {
      return function () {
         switch (_v12.ctor)
         {case "_Tuple2":
            return {ctor: "_Tuple2"
                   ,_0: _v12._0 * k
                   ,_1: _v12._1 * k};}
         _E.Case($moduleName,
         "on line 22, column 4 to 16");
      }();
   });
   var subVec = F2(function (_v16,
   _v17) {
      return function () {
         switch (_v17.ctor)
         {case "_Tuple2":
            return function () {
                 switch (_v16.ctor)
                 {case "_Tuple2":
                    return {ctor: "_Tuple2"
                           ,_0: _v16._0 - _v17._0
                           ,_1: _v16._1 - _v17._1};}
                 _E.Case($moduleName,
                 "on line 17, column 4 to 18");
              }();}
         _E.Case($moduleName,
         "on line 17, column 4 to 18");
      }();
   });
   var addVec = F2(function (_v24,
   _v25) {
      return function () {
         switch (_v25.ctor)
         {case "_Tuple2":
            return function () {
                 switch (_v24.ctor)
                 {case "_Tuple2":
                    return {ctor: "_Tuple2"
                           ,_0: _v24._0 + _v25._0
                           ,_1: _v24._1 + _v25._1};}
                 _E.Case($moduleName,
                 "on line 12, column 4 to 18");
              }();}
         _E.Case($moduleName,
         "on line 12, column 4 to 18");
      }();
   });
   var zero = {ctor: "_Tuple2"
              ,_0: 0
              ,_1: 0};
   _elm.Vector.values = {_op: _op
                        ,zero: zero
                        ,addVec: addVec
                        ,subVec: subVec
                        ,multVec: multVec
                        ,dotVec: dotVec
                        ,revVec: revVec
                        ,unit: unit
                        ,getx: getx
                        ,gety: gety
                        ,clampVec: clampVec};
   return _elm.Vector.values;
};