%%%-------------------------------------------------------------------
%%% @author lycaon
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%% ルームの管理を行う
%%% といっても最初のプレイヤーの振り分けのみ行って残りはルームプロセスに処理を任せる
%%% @end
%%% Created : 15. 9 2014 13:01
%%%-------------------------------------------------------------------
-module(mmmario_room_server).
-author("lycaon").

-include_lib("stdlib/include/ms_transform.hrl").

-behaviour(gen_server).

%% API
-export([
  start_link/0,
  stop/0,
  new_player/1,
  new_state/2,
  delete_room/1,
  all_room/0,
  all_room_count/0
]).

%% gen_server callbacks
-export([
  init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3
]).

-define(SERVER, ?MODULE).

-record(rsrvstate, {
}).

%%--------------------------------------------------------------------
%% @doc
%% 部屋情報
%% @end
%%--------------------------------------------------------------------
-record(rinfo, {
  pid, % 部屋のPID pid()
  state % 部屋の状態
}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% サーバー開始
%% @end
%%--------------------------------------------------------------------
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% @doc
%% 通常終了させる
%% @end
%%--------------------------------------------------------------------
stop() ->
  exit(whereis(?SERVER), normal).

%%--------------------------------------------------------------------
%% @doc
%% 新規プレイヤー
%% @end
%%--------------------------------------------------------------------
new_player(PUid) ->
  gen_server:call(?SERVER, {new_player, PUid}).

%%--------------------------------------------------------------------
%% @doc
%% 部屋の状態変更
%% @end
%%--------------------------------------------------------------------
new_state(RPid, State) ->
  gen_server:cast(?SERVER, {new_state, RPid, State}).

%%--------------------------------------------------------------------
%% @doc
%% 部屋の削除
%% @end
%%--------------------------------------------------------------------
delete_room(RUid) ->
  gen_server:call(?SERVER, {delete_room, RUid}).

%%--------------------------------------------------------------------
%% @doc
%% 全ての部屋PidをETSから取得
%% @end
%%--------------------------------------------------------------------
all_room() ->
  gen_server:call(?SERVER, all_room).

%%--------------------------------------------------------------------
%% @doc
%% 部屋総数を取得
%% @end
%%--------------------------------------------------------------------
all_room_count() ->
  gen_server:call(?SERVER, all_room_count).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% 初期化。ETSテーブルも作成する
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
  ?SERVER = ets:new(?SERVER, [set, named_table, {keypos, #rinfo.pid}]),
  {ok, #rsrvstate{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% 新規プレイヤーを空いてる部屋へ誘導する
%% @end
%%--------------------------------------------------------------------
handle_call({new_player, PUid}, _From, State) ->
  % etsでidle状態にある部屋がないかチェック
  RPids = ets:select(?SERVER, ets:fun2ms(
    fun(#rinfo{pid = RPid, state = RState})
      when idle == RState ->
      RPid
    end
  )),
  if
    0 < length(RPids) ->
      RPid = hd(RPids),
      error_logger:info_msg("Empty room[~p] found.~n", [RPid]),
      mmmario_room:new_player(RPid, PUid),
      {reply, RPid, State};
    true ->
      error_logger:info_msg("No empty room found.~n"),
      {ok, RPid} = mmmario_room_sup:start_room(),
      mmmario_room:new_player(RPid, PUid),
      ets:insert(?SERVER, #rinfo{pid = RPid, state = idle}),
      {reply, RPid, State}
  end;

%%--------------------------------------------------------------------
%% @doc
%% 全ての部屋を取得
%% @end
%%--------------------------------------------------------------------
handle_call(all_room, _From, State) ->
  RPids = ets:all(),
  {reply, RPids, State};

%%--------------------------------------------------------------------
%% @doc
%% 部屋総数を取得
%% @end
%%--------------------------------------------------------------------
handle_call(all_room_count, _From, State) ->
  Size = ets:info(?SERVER, size),
  {reply, Size, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% 部屋の状態変更
%% @end
%%--------------------------------------------------------------------
handle_cast({new_state, RPid, RState}, State) ->
  ets:insert(?SERVER, #rinfo{pid = RPid, state = RState}),
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% 終了時コールバック。ETSを終わらせておく
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
  ets:delete(?SERVER),
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================