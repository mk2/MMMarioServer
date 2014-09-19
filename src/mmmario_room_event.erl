%%%-------------------------------------------------------------------
%%% @author lycaon
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. 9 2014 1:16
%%%-------------------------------------------------------------------
-module(mmmario_room_event).
-author("lycaon").

-behaviour(gen_event).

%% API
-export([
  start_link/0,
  stop/1,
  add_handler/3,
  delete_handler/2,
  notice_ready/1,
  notice_rects/1
]).

%% gen_event callbacks
-export([
  init/1,
  handle_event/2,
  handle_call/2,
  handle_info/2,
  terminate/2,
  code_change/3
]).

-include_lib("stdlib/include/ms_transform.hrl").

-include("mmmario_room.hrl").

-define(SERVER, ?MODULE).

-record(roomevtstate, {
  ptid, % プレイヤー情報用ETSテーブルID
  puid % プレイヤーUID
}).

%%%===================================================================
%%% gen_event callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
start_link() ->
  {ok, EPid} = gen_event:start_link(),
  EPid.

%%--------------------------------------------------------------------
%% @doc
%% イベントマネージャをストップ
%% @end
%%--------------------------------------------------------------------
stop(EPid) ->
  gen_event:stop(EPid).

%%--------------------------------------------------------------------
%% @doc
%% ハンドラー追加
%% ユーザー情報のETSテーブルIDを引き数に取る
%% @end
%%--------------------------------------------------------------------
add_handler(EMPid, PTid, PUid) ->
  HandlerId = {?MODULE, make_ref()},
  gen_event:add_handler(EMPid, HandlerId, [PTid, PUid]),
  HandlerId.

%%--------------------------------------------------------------------
%% @doc
%% ハンドラーを削除
%% @end
%%--------------------------------------------------------------------
delete_handler(EMPid, HandlerId) ->
  gen_event:delete_handler(EMPid, HandlerId, []).

%%--------------------------------------------------------------------
%% @doc
%% プレイヤーにreadyメッセージを送信
%% @end
%%--------------------------------------------------------------------
notice_ready(EMPid) ->
  gen_event:notify(EMPid, ready).

%%--------------------------------------------------------------------
%% @doc
%% プレイヤーのレクトを通知する
%% @end
%%--------------------------------------------------------------------
notice_rects(EMPid) ->
  gen_event:notify(EMPid, rects).

%%%===================================================================
%%% gen_event callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
init([PTid, PUid]) ->
  {ok, #roomevtstate{ptid = PTid, puid = PUid}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% readyメッセージを送信
%% @end
%%--------------------------------------------------------------------
handle_event(ready, State = #roomevtstate{puid = PUid}) ->
  mmmario_player:ready_player(PUid),
  {ok, State};

%%--------------------------------------------------------------------
%% @private
%% @doc
%% rectsメッセージを送信
%% @end
%%--------------------------------------------------------------------
handle_event(rects, State = #roomevtstate{puid = PUid, ptid = PTid}) ->
  Rects = lists:flatten(ets:select(PTid, ets:fun2ms(
    fun(#cinfo{uid = CPUid, rect = Rect}) when CPUid =:= PUid ->
      Rect
    end
  ))),
  mmmario_player:move_other_players(PUid, Rects),
  {ok, State};

handle_event(_Event, State) ->
  {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, State) ->
  Reply = ok,
  {ok, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
  {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Arg, _State) ->
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
