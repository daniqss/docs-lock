-module(docs_lock_db_mongo_sup).

%%% BEHAVIOUR
-behaviour(supervisor).

%%% EXTERNAL EXPORTS
-export([start_link/0]).

%%% SUPERVISOR BEHAVIOR CALLBACKS
-export([init/1]).

%%% MACROS
-define(BD_NAME, <<"docs_lock">>).

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
    {ok, Connection} = mc_worker_api:connect([{database, ?BD_NAME}]),
    persistent_term:put(connection, Connection),
    Flags = #{
        strategy => one_for_one,
        intensity => 3,
        period => 5
    },
    {ok, {Flags, []}}.
