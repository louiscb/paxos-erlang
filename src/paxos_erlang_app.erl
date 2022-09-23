%%%-------------------------------------------------------------------
%% @doc paxos_erlang public API
%% @end
%%%-------------------------------------------------------------------

-module(paxos_erlang_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    paxos_erlang_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
