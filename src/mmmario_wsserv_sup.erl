%%%-------------------------------------------------------------------
%%% @author lycaon
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. 8 2014 22:32
%%%-------------------------------------------------------------------
-module(mmmario_wsserv_sup).
-behaviour(supervisor).
-author("lycaon").

%% API
-export([start_link/0, start_wsserv/0]).
-export([init/1]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  MaxR = 60,
  MaxT = 3600,
  % パッシブモードなのは、とりあえず開始時のハンドシェイクはパッシブにやりたいから。
  {ok, LSock} = gen_tcp:listen(8080, [binary, {active, false}, {packet, http}]),
  % あらかじめ待機状態にあるwsservを生成しておく。
  spawn_link(fun empty_wsservs/0),
  {ok, {{simple_one_for_one, MaxR, MaxT},
    [{
      wsserv,
      {mmmario_wsserv, start_link, [LSock]},
      temporary, 1000, worker, [mmmario_wsserv]
    }]}}.

start_wsserv() ->
  supervisor:start_child(?MODULE, []).

empty_wsservs() ->
  [start_wsserv() || _ <- lists:seq(1, 20)],
  ok.
