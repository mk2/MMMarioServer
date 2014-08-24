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
    var MMMarioConfig = Elm.MMMarioConfig.make(_elm);
    var MMMarioRenderer = Elm.MMMarioRenderer.make(_elm);
    var MMMarioType = Elm.MMMarioType.make(_elm);
    var MMMarioUtil = Elm.MMMarioUtil.make(_elm);
    var MMMarioVector = Elm.MMMarioVector.make(_elm);
    var Maybe = Elm.Maybe.make(_elm);
    var Native = Native || {};
    Native.Json = Elm.Native.Json.make(_elm);
    var Native = Native || {};
    Native.Ports = Elm.Native.Ports.make(_elm);
    var Signal = Elm.Signal.make(_elm);
    var String = Elm.String.make(_elm);
    var Text = Elm.Text.make(_elm);
    var Time = Elm.Time.make(_elm);
    var WebSocket = Elm.WebSocket.make(_elm);
    var Window = Elm.Window.make(_elm);
    var _op = {};
    var display = F2(function (windowSize, gameState) {
        return A2(MMMarioRenderer.render,
            windowSize,
            gameState);
    });
    var updateCharaImage = function (m) {
        return m;
    };
    var calcCharaPos = F2(function (delta, m) {
        return function () {
            var $ = A2(MMMarioVector.clampVec,
                    MMMarioConfig.minSpd,
                    MMMarioConfig.maxSpd)(MMMarioVector.addVec(m.spd)(MMMarioVector.multVec(delta)(m.acc))),
                sx = $._0,
                sy = $._1;
            var $ = A2(MMMarioVector.addVec,
                    m.pos,
                    {ctor: "_Tuple2", _0: sx, _1: sy}),
                nx = $._0,
                ny = $._1;
            return _U.cmp(ny,
                0) < 0 ? _U.replace([
                    ["pos"
                        , A3(MMMarioVector.clampVec,
                        MMMarioConfig.minPos,
                        MMMarioConfig.maxPos,
                        {ctor: "_Tuple2", _0: nx, _1: 0})]
                    ,
                    ["spd"
                        , {ctor: "_Tuple2", _0: sx, _1: 0}]
                    ,
                    ["isTouchOnDownBlock", true]
                ],
                m) : _U.replace([
                    ["pos"
                        , A3(MMMarioVector.clampVec,
                        MMMarioConfig.minPos,
                        MMMarioConfig.maxPos,
                        {ctor: "_Tuple2", _0: nx, _1: ny})]
                    ,
                    ["spd"
                        , {ctor: "_Tuple2", _0: sx, _1: sy}]
                ],
                m);
        }();
    });
    var calcCharaAccel = F6(function (delta, moveAccel, fricAccel, gravityAccel, willJump, m) {
        return function () {
            var jumpable = willJump && m.isTouchOnDownBlock;
            return jumpable ? _U.replace([
                    ["acc"
                        , MMMarioConfig.marioJumpAccel]
                ],
                m) : Basics.not(m.isTouchOnDownBlock) ? _U.replace([
                    ["acc"
                        , A2(MMMarioVector.addVec,
                        fricAccel,
                        gravityAccel)]
                ],
                m) : _U.replace([
                    ["acc"
                        , MMMarioVector.addVec(moveAccel)(MMMarioVector.addVec(fricAccel)(gravityAccel))]
                ],
                m);
        }();
    });
    var stepGame = F2(function (_v0, gameState) {
        return function () {
            switch (_v0.ctor) {
                case "_Tuple5":
                    return function () {
                        var maybeFloat = function ($) {
                            return A2(Maybe.maybe,
                                0.0,
                                function (n) {
                                    return n;
                                })(String.toFloat($));
                        };
                        var cnvToFloat = function (_v7) {
                            return function () {
                                switch (_v7.ctor) {
                                    case "::":
                                        switch (_v7._1.ctor) {
                                            case "::":
                                                switch (_v7._1._1.ctor) {
                                                    case "::":
                                                        switch (_v7._1._1._1.ctor) {
                                                            case "[]":
                                                                return {ctor: "_Tuple2", _0: _v7._0, _1: {ctor: "_Tuple2", _0: maybeFloat(_v7._1._0), _1: maybeFloat(_v7._1._1._0)}};
                                                        }
                                                        break;
                                                }
                                                break;
                                        }
                                        break;
                                }
                                _E.Case($moduleName,
                                    "on line 132, column 44 to 84");
                            }();
                        };
                        var poss = A2(String.split,
                            ",",
                            _v0._3);
                        var numCharas = List.length(poss) / 2 | 0;
                        var otherCharas = Debug.log("otherCharas")(List.zip(_L.range(1,
                            numCharas))(List.map(cnvToFloat)(A2(MMMarioUtil.takeCycleAsList,
                            3,
                            poss))));
                        var moveAccel = A2(MMMarioVector.multVec,
                            MMMarioConfig.moveCoeff,
                            {ctor: "_Tuple2", _0: Basics.toFloat(_v0._1.x), _1: 0});
                        var preMario = gameState.mario;
                        var fricAccel = A2(MMMarioVector.multVec,
                            MMMarioConfig.fricCoeff,
                            MMMarioVector.revVec(preMario.spd));
                        var updateChara = function ($) {
                            return updateCharaImage(calcCharaPos(_v0._0)(A5(calcCharaAccel,
                                _v0._0,
                                moveAccel,
                                fricAccel,
                                MMMarioConfig.gravityAccel,
                                _v0._2)($)));
                        };
                        var newMario = updateChara(preMario);
                        var marioPosStr = _L.append("M",
                            _L.append(String.show(MMMarioUtil.absRound(MMMarioVector.getx(newMario.pos))),
                                _L.append(",",
                                    String.show(MMMarioUtil.absRound(MMMarioVector.gety(newMario.pos))))));
                        return _U.replace([
                                ["mario"
                                    , newMario]
                                ,
                                ["sendData", marioPosStr]
                                ,
                                ["otherCharas", otherCharas]
                                ,
                                ["clientName", _v0._4]
                            ],
                            gameState);
                    }();
            }
            _E.Case($moduleName,
                "between lines 109 and 138");
        }();
    });
    var sendData = function (gameState) {
        return gameState.sendData;
    };
    var clientName = Native.Ports.portIn("clientName",
        Native.Ports.incomingSignal(function (v) {
            return typeof v === "string" || typeof v === "object" && v instanceof String ? v : _E.raise("invalid input, expecting JSString but got " + v);
        }));
    var wsRecvData = Native.Ports.portIn("wsRecvData",
        Native.Ports.incomingSignal(function (v) {
            return typeof v === "string" || typeof v === "object" && v instanceof String ? v : _E.raise("invalid input, expecting JSString but got " + v);
        }));
    var inputSignal = function () {
        var delta = A2(Signal._op["<~"],
            Time.inSeconds,
            Time.fps(MMMarioConfig.gameFps));
        var keySignal = A2(Signal._op["~"],
            A2(Signal._op["~"],
                A2(Signal._op["~"],
                    A2(Signal._op["~"],
                        A2(Signal._op["<~"],
                            F5(function (v0, v1, v2, v3, v4) {
                                return {ctor: "_Tuple5", _0: v0, _1: v1, _2: v2, _3: v3, _4: v4};
                            }),
                            delta),
                        Keyboard.arrows),
                    Keyboard.space),
                wsRecvData),
            clientName);
        return A2(Signal.sampleOn,
            delta,
            keySignal);
    }();
    var gameStateSignal = A3(Signal.foldp,
        stepGame,
        MMMarioConfig.initialGameState,
        inputSignal);
    var wsSendData = Native.Ports.portOut("wsSendData",
        Native.Ports.outgoingSignal(function (v) {
            return v;
        }),
        function () {
            var delta = A2(Signal._op["<~"],
                Time.inSeconds,
                Time.fps(MMMarioConfig.requestFps));
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
    _elm.MMMarioClient.values = {_op: _op, inputSignal: inputSignal, gameStateSignal: gameStateSignal, sendData: sendData, calcCharaAccel: calcCharaAccel, calcCharaPos: calcCharaPos, updateCharaImage: updateCharaImage, stepGame: stepGame, display: display, main: main};
    return _elm.MMMarioClient.values;
};
Elm.MMMarioUtil = Elm.MMMarioUtil || {};
Elm.MMMarioUtil.make = function (_elm) {
    "use strict";
    _elm.MMMarioUtil = _elm.MMMarioUtil || {};
    if (_elm.MMMarioUtil.values)
        return _elm.MMMarioUtil.values;
    var _N = Elm.Native,
        _U = _N.Utils.make(_elm),
        _L = _N.List.make(_elm),
        _A = _N.Array.make(_elm),
        _E = _N.Error.make(_elm),
        $moduleName = "MMMarioUtil";
    var Basics = Elm.Basics.make(_elm);
    var Color = Elm.Color.make(_elm);
    var Graphics = Graphics || {};
    Graphics.Collage = Elm.Graphics.Collage.make(_elm);
    var Graphics = Graphics || {};
    Graphics.Element = Elm.Graphics.Element.make(_elm);
    var List = Elm.List.make(_elm);
    var MMMarioConfig = Elm.MMMarioConfig.make(_elm);
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
    var getTileYCoord = function (pos) {
        return pos / MMMarioConfig.tileHeight | 0;
    };
    var getTileXCoord = function (pos) {
        return pos / MMMarioConfig.tileWidth | 0;
    };
    var absRound = function ($) {
        return Basics.round(Basics.abs($));
    };
    var takeCycleAsList$ = F3(function (n, l, accum) {
        return function () {
            var notEnough = _U.cmp(List.length(l),
                n) < 0;
            return notEnough ? accum : A3(takeCycleAsList$,
                n,
                A2(List.drop, n, l),
                _L.append(accum,
                    _L.fromArray([A2(List.take,
                        n,
                        l)])));
        }();
    });
    var takeCycleAsList = F2(function (n, l) {
        return A3(takeCycleAsList$,
            n,
            l,
            _L.fromArray([]));
    });
    var list2tuple = function (l) {
        return {ctor: "_Tuple2", _0: List.head(l), _1: List.last(l)};
    };
    var takeCycle$ = F3(function (n, l, accum) {
        return function () {
            var notEnough = _U.cmp(List.length(l),
                n) < 0;
            return notEnough ? accum : A3(takeCycle$,
                n,
                A2(List.drop, n, l),
                _L.append(accum,
                    _L.fromArray([list2tuple(A2(List.take,
                        n,
                        l))])));
        }();
    });
    var takeCycle = F2(function (n, l) {
        return A3(takeCycle$,
            n,
            l,
            _L.fromArray([]));
    });
    _elm.MMMarioUtil.values = {_op: _op, list2tuple: list2tuple, takeCycle: takeCycle, takeCycle$: takeCycle$, takeCycleAsList: takeCycleAsList, takeCycleAsList$: takeCycleAsList$, absRound: absRound, getTileXCoord: getTileXCoord, getTileYCoord: getTileYCoord};
    return _elm.MMMarioUtil.values;
};
Elm.MMMarioRenderer = Elm.MMMarioRenderer || {};
Elm.MMMarioRenderer.make = function (_elm) {
    "use strict";
    _elm.MMMarioRenderer = _elm.MMMarioRenderer || {};
    if (_elm.MMMarioRenderer.values)
        return _elm.MMMarioRenderer.values;
    var _N = Elm.Native,
        _U = _N.Utils.make(_elm),
        _L = _N.List.make(_elm),
        _A = _N.Array.make(_elm),
        _E = _N.Error.make(_elm),
        $moduleName = "MMMarioRenderer";
    var Array = Elm.Array.make(_elm);
    var Basics = Elm.Basics.make(_elm);
    var Color = Elm.Color.make(_elm);
    var Graphics = Graphics || {};
    Graphics.Collage = Elm.Graphics.Collage.make(_elm);
    var Graphics = Graphics || {};
    Graphics.Element = Elm.Graphics.Element.make(_elm);
    var List = Elm.List.make(_elm);
    var MMMarioConfig = Elm.MMMarioConfig.make(_elm);
    var MMMarioType = Elm.MMMarioType.make(_elm);
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
    var renderTile = F2(function (moveXY, stageTile) {
        return function () {
            switch (stageTile.ctor) {
                case "Cloud":
                    return Maybe.Just(Graphics.Collage.move(moveXY)(Graphics.Collage.filled(MMMarioConfig.cldColor)(A2(Graphics.Collage.rect,
                        Basics.toFloat(MMMarioConfig.tileWidth),
                        Basics.toFloat(MMMarioConfig.tileHeight)))));
                case "Ground":
                    return Maybe.Just(Graphics.Collage.move(moveXY)(Graphics.Collage.filled(MMMarioConfig.gndColor)(A2(Graphics.Collage.rect,
                        Basics.toFloat(MMMarioConfig.tileWidth),
                        Basics.toFloat(MMMarioConfig.tileHeight)))));
                case "None":
                    return Maybe.Nothing;
            }
            return Maybe.Nothing;
        }();
    });
    var renderTileRow = F2(function (row, stageTiles) {
        return function () {
            var my = Basics.toFloat(MMMarioConfig.tileHeight * row);
            return Array.toList(A2(Array.indexedMap,
                F2(function (mx, stageTile) {
                    return A2(renderTile,
                        {ctor: "_Tuple2", _0: Basics.toFloat(MMMarioConfig.tileWidth * mx), _1: my},
                        stageTile);
                }),
                Array.fromList(stageTiles)));
        }();
    });
    var renderStage = F2(function (_v1, stageTileRows) {
        return function () {
            switch (_v1.ctor) {
                case "_Tuple2":
                    return Graphics.Collage.group(Array.toList(A2(Array.indexedMap,
                        F2(function (row, stageTileRow) {
                            return Graphics.Collage.group(Maybe.justs(A2(renderTileRow,
                                row,
                                stageTileRow)));
                        }),
                        Array.fromList(stageTileRows))));
            }
            _E.Case($moduleName,
                "on line 48, column 5 to 128");
        }();
    });
    var getImage = F2(function (chara, _v5) {
        return function () {
            switch (_v5.ctor) {
                case "_Tuple2":
                    return A3(Graphics.Element.image,
                        _v5._0,
                        _v5._1,
                        List.concat(_L.fromArray([MMMarioConfig.imageBaseUrl
                            , chara.imageBaseName
                            , "-"
                            , chara.imagePoseName
                            , "-"
                            , chara.imageDireName
                            , ".png"])));
            }
            _E.Case($moduleName,
                "on line 11, column 3 to 116");
        }();
    });
    var render = F2(function (_v9, gameState) {
        return function () {
            switch (_v9.ctor) {
                case "_Tuple2":
                    return function () {
                        var moveToY = Basics.toFloat((0 - _v9._1) / 2 | 0);
                        var moveToX = Basics.toFloat((0 - _v9._0) / 2 | 0);
                        var stageForm = A2(renderStage,
                            {ctor: "_Tuple2", _0: 0, _1: 0},
                            gameState.stageTiles);
                        var bgForm = Graphics.Collage.filled(MMMarioConfig.bgColor)(A2(Graphics.Collage.rect,
                            Basics.toFloat(_v9._0),
                            Basics.toFloat(_v9._1)));
                        var screenTileHeight = _v9._1 / MMMarioConfig.tileHeight | 0;
                        var screenTileWidth = _v9._0 / MMMarioConfig.tileWidth | 0;
                        var lastGameState = _U.replace([
                                ["screenTileWidth"
                                    , screenTileWidth]
                                ,
                                ["screenTileHeight"
                                    , screenTileHeight]
                            ],
                            gameState);
                        var marioImage = A2(getImage,
                            lastGameState.mario,
                            {ctor: "_Tuple2", _0: 20, _1: 35});
                        var drawMario = function (_v13) {
                            return function () {
                                switch (_v13.ctor) {
                                    case "_Tuple2":
                                        switch (_v13._1.ctor) {
                                            case "_Tuple2":
                                                return Graphics.Collage.move(_v13._1._1)(Graphics.Collage.toForm(marioImage));
                                        }
                                        break;
                                }
                                _E.Case($moduleName,
                                    "on line 34, column 43 to 73");
                            }();
                        };
                        var marioForm = Graphics.Collage.move(lastGameState.mario.pos)(Graphics.Collage.toForm(marioImage));
                        var clientName = gameState.clientName;
                        var filterMario = function (_v19) {
                            return function () {
                                switch (_v19.ctor) {
                                    case "_Tuple2":
                                        switch (_v19._1.ctor) {
                                            case "_Tuple2":
                                                return !_U.eq(_v19._1._0,
                                                    clientName);
                                        }
                                        break;
                                }
                                _E.Case($moduleName,
                                    "on line 35, column 45 to 63");
                            }();
                        };
                        var marioForms = Graphics.Collage.group(List.map(drawMario)(A2(List.filter,
                            filterMario,
                            gameState.otherCharas)));
                        var stageWholeForm = Graphics.Collage.move({ctor: "_Tuple2", _0: moveToX, _1: moveToY})(Graphics.Collage.group(_L.fromArray([stageForm
                            , marioForm
                            , marioForms])));
                        return A3(Graphics.Collage.collage,
                            _v9._0,
                            _v9._1,
                            _L.fromArray([bgForm
                                , stageWholeForm]));
                    }();
            }
            _E.Case($moduleName,
                "between lines 16 and 44");
        }();
    });
    _elm.MMMarioRenderer.values = {_op: _op, getImage: getImage, render: render, renderStage: renderStage, renderTileRow: renderTileRow, renderTile: renderTile};
    return _elm.MMMarioRenderer.values;
};
Elm.MMMarioConfig = Elm.MMMarioConfig || {};
Elm.MMMarioConfig.make = function (_elm) {
    "use strict";
    _elm.MMMarioConfig = _elm.MMMarioConfig || {};
    if (_elm.MMMarioConfig.values)
        return _elm.MMMarioConfig.values;
    var _N = Elm.Native,
        _U = _N.Utils.make(_elm),
        _L = _N.List.make(_elm),
        _A = _N.Array.make(_elm),
        _E = _N.Error.make(_elm),
        $moduleName = "MMMarioConfig";
    var Basics = Elm.Basics.make(_elm);
    var Color = Elm.Color.make(_elm);
    var Graphics = Graphics || {};
    Graphics.Collage = Elm.Graphics.Collage.make(_elm);
    var Graphics = Graphics || {};
    Graphics.Element = Elm.Graphics.Element.make(_elm);
    var List = Elm.List.make(_elm);
    var MMMarioType = Elm.MMMarioType.make(_elm);
    var MMMarioVector = Elm.MMMarioVector.make(_elm);
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
    var moveCoeff = 500;
    var fricCoeff = 10;
    var gravityAccel = {ctor: "_Tuple2", _0: 0, _1: -900};
    var marioJumpAccel = {ctor: "_Tuple2", _0: 0, _1: 2000};
    var defaultChara = {_: {}, acc: MMMarioVector.zero, imageBaseName: "", imageDireName: "", imagePoseName: "", isTouchOnDownBlock: false, isTouchOnLeftBlock: false, isTouchOnRightBlock: false, isTouchOnTopBlock: false, mass: 100, pos: MMMarioVector.zero, spd: MMMarioVector.zero};
    var minSpd = {ctor: "_Tuple2", _0: -10, _1: -10};
    var maxSpd = {ctor: "_Tuple2", _0: 10, _1: 10};
    var minPos = {ctor: "_Tuple2", _0: 0, _1: 0};
    var maxPos = {ctor: "_Tuple2", _0: 10000, _1: 10000};
    var resourceBaseUrl = "resources/";
    var imageBaseUrl = _L.append(resourceBaseUrl,
        "images/");
    var tileHeight = 64;
    var tileWidth = 64;
    var requestFps = 5;
    var gameFps = 60;
    var gndColor = A3(Color.rgb,
        188,
        118,
        71);
    var cldColor = A3(Color.rgb,
        255,
        255,
        255);
    var bgColor = A3(Color.rgb,
        160,
        216,
        239);
    var sampleStageTiles = _L.append(_L.fromArray([A2(List.repeat,
            10,
            MMMarioType.Ground)]),
        List.repeat(9)(List.repeat(10)(MMMarioType.None)));
    var initialGameState = {_: {}, clientName: "", mario: _U.replace([
            ["pos"
                , {ctor: "_Tuple2", _0: 0, _1: 100}]
            ,
            ["imageBaseName", "mario"]
            ,
            ["imagePoseName", "stand"]
            ,
            ["imageDireName", "right"]
        ],
        defaultChara), otherCharas: _L.fromArray([]), screenTileHeight: 10, screenTileWidth: 10, sendData: "", stageTileHeight: 100, stageTileWidth: 200, stageTiles: sampleStageTiles};
    _elm.MMMarioConfig.values = {_op: _op, sampleStageTiles: sampleStageTiles, bgColor: bgColor, cldColor: cldColor, gndColor: gndColor, gameFps: gameFps, requestFps: requestFps, tileWidth: tileWidth, tileHeight: tileHeight, resourceBaseUrl: resourceBaseUrl, imageBaseUrl: imageBaseUrl, maxPos: maxPos, minPos: minPos, maxSpd: maxSpd, minSpd: minSpd, defaultChara: defaultChara, initialGameState: initialGameState, marioJumpAccel: marioJumpAccel, gravityAccel: gravityAccel, fricCoeff: fricCoeff, moveCoeff: moveCoeff};
    return _elm.MMMarioConfig.values;
};
Elm.MMMarioType = Elm.MMMarioType || {};
Elm.MMMarioType.make = function (_elm) {
    "use strict";
    _elm.MMMarioType = _elm.MMMarioType || {};
    if (_elm.MMMarioType.values)
        return _elm.MMMarioType.values;
    var _N = Elm.Native,
        _U = _N.Utils.make(_elm),
        _L = _N.List.make(_elm),
        _A = _N.Array.make(_elm),
        _E = _N.Error.make(_elm),
        $moduleName = "MMMarioType";
    var Basics = Elm.Basics.make(_elm);
    var Color = Elm.Color.make(_elm);
    var Graphics = Graphics || {};
    Graphics.Collage = Elm.Graphics.Collage.make(_elm);
    var Graphics = Graphics || {};
    Graphics.Element = Elm.Graphics.Element.make(_elm);
    var List = Elm.List.make(_elm);
    var MMMarioVector = Elm.MMMarioVector.make(_elm);
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
    var GameState = F9(function (a, b, c, d, e, f, g, h, i) {
        return {_: {}, clientName: i, mario: a, otherCharas: g, screenTileHeight: f, screenTileWidth: e, sendData: h, stageTileHeight: c, stageTileWidth: b, stageTiles: d};
    });
    var Chara = function (a) {
        return function (b) {
            return function (c) {
                return function (d) {
                    return function (e) {
                        return function (f) {
                            return function (g) {
                                return function (h) {
                                    return function (i) {
                                        return function (j) {
                                            return function (k) {
                                                return {_: {}, acc: c, imageBaseName: i, imageDireName: k, imagePoseName: j, isTouchOnDownBlock: f, isTouchOnLeftBlock: e, isTouchOnRightBlock: g, isTouchOnTopBlock: d, mass: h, pos: a, spd: b};
                                            };
                                        };
                                    };
                                };
                            };
                        };
                    };
                };
            };
        };
    };
    var UserInput = F2(function (a, b) {
        return {_: {}, arr: a, space: b};
    });
    var Ground = {ctor: "Ground"};
    var Cloud = {ctor: "Cloud"};
    var None = {ctor: "None"};
    var RItem = {ctor: "RItem"};
    var RBlock = {ctor: "RBlock"};
    var RChara = {ctor: "RChara"};
    _elm.MMMarioType.values = {_op: _op, RChara: RChara, RBlock: RBlock, RItem: RItem, None: None, Cloud: Cloud, Ground: Ground, UserInput: UserInput, Chara: Chara, GameState: GameState};
    return _elm.MMMarioType.values;
};
Elm.MMMarioVector = Elm.MMMarioVector || {};
Elm.MMMarioVector.make = function (_elm) {
    "use strict";
    _elm.MMMarioVector = _elm.MMMarioVector || {};
    if (_elm.MMMarioVector.values)
        return _elm.MMMarioVector.values;
    var _N = Elm.Native,
        _U = _N.Utils.make(_elm),
        _L = _N.List.make(_elm),
        _A = _N.Array.make(_elm),
        _E = _N.Error.make(_elm),
        $moduleName = "MMMarioVector";
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
    var clampVec = F3(function (minVec, maxVec, vec) {
        return function () {
            var minY = gety(minVec);
            var minX = getx(minVec);
            var maxY = gety(maxVec);
            var maxX = getx(maxVec);
            var y = gety(vec);
            var x = getx(vec);
            return {ctor: "_Tuple2", _0: A3(Basics.clamp,
                minX,
                maxX,
                x), _1: A3(Basics.clamp,
                minY,
                maxY,
                y)};
        }();
    });
    var unit = {ctor: "_Tuple2", _0: 1, _1: 1};
    var revVec = function (_v0) {
        return function () {
            switch (_v0.ctor) {
                case "_Tuple2":
                    return {ctor: "_Tuple2", _0: 0 - _v0._0, _1: 0 - _v0._1};
            }
            _E.Case($moduleName,
                "on line 32, column 4 to 10");
        }();
    };
    var dotVec = F2(function (_v4, _v5) {
        return function () {
            switch (_v5.ctor) {
                case "_Tuple2":
                    return function () {
                        switch (_v4.ctor) {
                            case "_Tuple2":
                                return _v4._0 * _v5._1 + _v4._1 * _v5._0;
                        }
                        _E.Case($moduleName,
                            "on line 27, column 3 to 18");
                    }();
            }
            _E.Case($moduleName,
                "on line 27, column 3 to 18");
        }();
    });
    var multVec = F2(function (k, _v12) {
        return function () {
            switch (_v12.ctor) {
                case "_Tuple2":
                    return {ctor: "_Tuple2", _0: _v12._0 * k, _1: _v12._1 * k};
            }
            _E.Case($moduleName,
                "on line 22, column 4 to 16");
        }();
    });
    var subVec = F2(function (_v16, _v17) {
        return function () {
            switch (_v17.ctor) {
                case "_Tuple2":
                    return function () {
                        switch (_v16.ctor) {
                            case "_Tuple2":
                                return {ctor: "_Tuple2", _0: _v16._0 - _v17._0, _1: _v16._1 - _v17._1};
                        }
                        _E.Case($moduleName,
                            "on line 17, column 4 to 18");
                    }();
            }
            _E.Case($moduleName,
                "on line 17, column 4 to 18");
        }();
    });
    var addVec = F2(function (_v24, _v25) {
        return function () {
            switch (_v25.ctor) {
                case "_Tuple2":
                    return function () {
                        switch (_v24.ctor) {
                            case "_Tuple2":
                                return {ctor: "_Tuple2", _0: _v24._0 + _v25._0, _1: _v24._1 + _v25._1};
                        }
                        _E.Case($moduleName,
                            "on line 12, column 4 to 18");
                    }();
            }
            _E.Case($moduleName,
                "on line 12, column 4 to 18");
        }();
    });
    var zero = {ctor: "_Tuple2", _0: 0, _1: 0};
    _elm.MMMarioVector.values = {_op: _op, zero: zero, addVec: addVec, subVec: subVec, multVec: multVec, dotVec: dotVec, revVec: revVec, unit: unit, getx: getx, gety: gety, clampVec: clampVec};
    return _elm.MMMarioVector.values;
};