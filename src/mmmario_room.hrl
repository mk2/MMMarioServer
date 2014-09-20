%%%-------------------------------------------------------------------
%%% @author lycaon
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. 9 2014 2:32
%%%-------------------------------------------------------------------
-author("lycaon").

-include("mmmario_game_type.hrl").

%%--------------------------------------------------------------------
%% @doc
%% キャラクター情報レコード。ETSで使う
%% @end
%%--------------------------------------------------------------------
-record(cinfo, {
  uid, % プレイヤーのUID {pid(), Name}
  hid, % イベントハンドラID
  name, % プレイヤー名 Name
  rect = ?UNIT_RECT, % キャラクターのレクト rect()
  state = alive % キャラクターの状態 alive|dead
}).