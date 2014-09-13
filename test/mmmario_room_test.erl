%%%-------------------------------------------------------------------
%%% @author lycaon
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 13. 9 2014 11:45
%%%-------------------------------------------------------------------
-module(mmmario_room_test).
-author("lycaon").

-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
setup_room_sup() ->
  mmmario_room_sup:start_link().

simple_test() ->
  ?assert(true).
