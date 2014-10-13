%%%-------------------------------------------------------------------
%%% @author lycaon
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. 9 2014 2:32
%%%-------------------------------------------------------------------
-author("lycaon").

-include("mmmario_common_type.hrl").
-include("mmmario_game_type.hrl").

%%--------------------------------------------------------------------
%% @doc
%% キャラクター情報レコード。ETSで使う
%% @end
%%--------------------------------------------------------------------
-record(cinfo, {
  uid :: puid(), % プレイヤーのUID {pid(), Name}
  hid :: hid(), % イベントハンドラID
  name :: string(), % プレイヤー名 Name
  rect = ?UNIT_RECT :: rect(), % キャラクターのレクト rect()
  state = alive :: alive | dead % キャラクターの状態 alive|dead
}).