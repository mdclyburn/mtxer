%%%-------------------------------------------------------------------
%% @doc mtxer public API
%% @end
%%%-------------------------------------------------------------------

-module(mtxer_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    mtxer_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
