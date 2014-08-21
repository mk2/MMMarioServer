%%%-------------------------------------------------------------------
%%% @author nijibox
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. 8 2014 14:52
%%%-------------------------------------------------------------------
-module(mmmario_test).
-author("lycaon").

-include_lib("eunit/include/eunit.hrl").

gen_wserv_test_() ->
  {"WebSocketサーバー生成テスト",
    {}}.

setup() ->
  mmmario:start(0, 0).