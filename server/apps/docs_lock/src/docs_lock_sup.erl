-module(docs_lock_sup).

%%% BEHAVIOUR
-behaviour(supervisor).

%%% EXTERNAL EXPORTS
-export([start_link/0]).

%%% SUPERVISOR BEHAVIOR CALLBACKS
-export([init/1]).

%%%-----------------------------------------------------------------------------
%%% EXTERNAL EXPORTS
%%%-----------------------------------------------------------------------------
-spec start_link() -> supervisor:startlink_ret().
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%%-----------------------------------------------------------------------------
%%% EXTERNAL EXPORTS
%%%-----------------------------------------------------------------------------
init([]) ->
    Flags = #{
        strategy => one_for_one,
        intensity => 3,
        period => 5
    },
    {ok, {Flags, []}}.
