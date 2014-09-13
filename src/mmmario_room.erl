%%%-------------------------------------------------------------------
%%% @author lycaon
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%% 部屋状態を表現するモジュール
%%% 1. idle状態 -> 部屋生成後、まだ最大収容人数に達していない場合の状態
%%% 2. pregame状態 -> 最大収容人数に達した後の遷移する状態。ここでゲーム開始前のチェックを行う
%%% 3. ongame状態 -> ゲーム中状態。ゲームの内部状態は別のモジュールで保持するので、ここでは突然接続が切れたプレイヤーの管理だけを行う。
%%% 4. postgame状態 -> ゲームが終わったら遷移する状態。ゲームの終了処理を行う。この状態の後、部屋プロセスはkillされる。
%%% @end
%%% Created : 08. 9 2014 23:43
%%%-------------------------------------------------------------------
-module(mmmario_room).
-author("lycaon").

-behaviour(gen_fsm).

%% API
-export([
  start_link/0,
  new_player/1
]).

%% gen_fsm callbacks
-export([
  init/1,
  idle/2,
  pregame/2,
  ongame/2,
  postgame/2,
  handle_event/3,
  handle_sync_event/4,
  handle_info/3,
  terminate/3,
  code_change/4,
  format_status/2
]).

-define(SERVER, ?MODULE).

-record(roomstate, {
  uid, % 部屋のUID {self(), make_ref()}
  pcount = 0, % 部屋にいる(はずの)プレイヤー数
  rcount = 0, % ゲーム開始直前のプレイヤー数(ゲーム開始以降この属性は固定になる)
  players = #{} % プレイヤーのUIDリスト, #{PUid => [{state, alive}]}
}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% ルームを開始する
%% @end
%%--------------------------------------------------------------------
start_link() ->
  gen_fsm:start_link({local, ?SERVER}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% @doc
%% 新規プレイヤー参上
%% @end
%%--------------------------------------------------------------------
new_player(PUid) ->
  gen_fsm:send_event(?SERVER, {new_player, PUid}).

%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% 部屋を開始する。最初の状態はidle
%% @end
%%--------------------------------------------------------------------
init([]) ->
  process_flag(trap_exit, true),
  RUid = {self(), make_ref()},
  mmmario_room_event_handler:notify({new_room, RUid}),
  {ok, idle, #roomstate{uid = RUid}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% 待ち状態。
%% プレイヤー追加イベントが来たら追加する
%% @end
%%--------------------------------------------------------------------
idle({new_player, PUid}, State = #roomstate{uid = Ruid, pcount = PCount, players = PUids}) ->
  MaxPCount = application:get_env(mmmario, maxpcount, 6),
  if
    PCount =:= MaxPCount ->
      error_logger:format("Room[~p] is full of players.~n", [self()]),
      mmmario_room_event_handler:notify({full_room, Ruid}),
      {next_state, pregame, State};
    PCount >= 0 andalso PCount < MaxPCount ->
      {next_state, idle, State#roomstate{pcount = PCount + 1, players = maps:put(PUid, [{state, alive}], PUids)}};
    true -> exit(self()) % 何かやばいエラーが起きているので終了する
  end;

%%--------------------------------------------------------------------
%% @private
%% @doc
%% 待ち状態。
%% プレイヤー離脱イベントが来たら削除する
%% @end
%%--------------------------------------------------------------------
idle({exit_player, PUid}, State = #roomstate{pcount = PCount, players = PUids}) ->
  if
    PCount >= 1 -> {next_state, idle, State#roomstate{pcount = PCount - 1, players = maps:remove(PUid, PUids)}};
    true -> {next_state, idle, State}
  end.

%%--------------------------------------------------------------------
%% @doc
%% ゲーム開始前状態。
%% 全てのプレイヤーの準備ができたらゲーム状態に移る
%% 同期呼び出し
%% @end
%%--------------------------------------------------------------------
pregame({player_ready, PUid}, State = #roomstate{uid = RUid, pcount = PCount, rcount = RCount, players = PUids}) ->
  if
    PCount =:= RCount ->
      error_logger:info_msg("Room[~p] All players are ready to game!!~n", [RUid]),
      {next_state, ongame, State};
    RCount >= 0 andalso RCount < PCount -> State#roomstate{rcount = RCount + 1};
    true -> State
  end.

%%--------------------------------------------------------------------
%% @doc
%% ゲーム中状態。
%% 突然抜けたプレイヤーに対応する
%% @end
%%--------------------------------------------------------------------
ongame({exit_player, PUid}, State = #roomstate{uid = RUid, pcount = PCount, players = PUids}) ->
  if
    PCount > 0 ->
      error_logger:info_msg("The player [~p] is exit.~n", [PUid]),
      State#roomstate{pcount = PCount - 1, players = maps:remove(PUid, PUids)};
    true ->
      error_logger:error_msg("No body in room.~n"),
      mmmario_room_event_handler:notify({abnormal_end, RUid}),
      {next_state, postgame, State}
  end;

%%--------------------------------------------------------------------
%% @doc
%% ゲーム中状態。
%% 負けたプレイヤーに対応する
%% @end
%%--------------------------------------------------------------------
ongame({die_player, PUid}, State = #roomstate{uid = RUid, pcount = PCount, players = PUids}) ->
  if
    PCount > 0 ->
      error_logger:info_msg("The player [~p] dies.~n", [PUid]),
      State#roomstate{pcount = PCount - 1, players = maps:update(PUid, [{state, dead}], PUids)};
    PCount =:= 1 ->
      ExtractAlivePlayerFun = fun(K, V, Acc) when V =:= [{state, alive}] -> Acc = K end,
      AlivePUid = maps:fold(ExtractAlivePlayerFun, undefined, PUids),
      error_logger:info_msg("The player [~p] wins at the room [~p].~n", [AlivePUid, RUid]),
      {next_state, postgame, State#roomstate{}};
    true ->
      error_logger:error_msg("No body in room."),
      mmmario_room_event_handler:notify({abnormal_end, RUid}),
      {next_state, postgame, State#roomstate{pcount = 0, players = []}}
  end.

%%--------------------------------------------------------------------
%% @doc
%% ゲーム終了状態。
%% @end
%%--------------------------------------------------------------------
postgame({get_result, Name}, State = #roomstate{pcount = PCount, players = PUids}) ->
  {next_state, ongame, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
handle_event(_Event, StateName, State) ->
  {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% @doc
%% stopイベントの処理
%% @end
%%--------------------------------------------------------------------
handle_sync_event(stop, _From, StateName, State) ->
  Reply = ok,
  {reply, Reply, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, StateName, State) ->
  {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _StateName, _State) ->
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, StateName, State, _Extra) ->
  {ok, StateName, State}.

%%--------------------------------------------------------------------
%% @doc
%% 主にユニットテスト用…
%% @end
%%--------------------------------------------------------------------
format_status(normal, [PDict, StatusData]) ->
  io:format("PDict: ~p~n", [PDict]),
  io:format("StatusData: ~p~n", [StatusData]),
  [{data, [
    {"PCount", StatusData#roomstate.pcount},
    {"RCount", StatusData#roomstate.rcount}]}].

%%%===================================================================
%%% Internal functions
%%%===================================================================
