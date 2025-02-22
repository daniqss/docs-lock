-module(docs_lock_service_rest).

%%% BEHAVIOURS
-behaviour(application).

%%% START/STOP EXPORTS
-export([
    start/2,
    stop/1
]).

%%%-----------------------------------------------------------------------------
%%% START/STOP EXPORTS
%%%-----------------------------------------------------------------------------
start(_StartType, _StartArgs) ->
    docs_lock_service_rest_sup:start_link().

stop(_St) ->
    ok.
