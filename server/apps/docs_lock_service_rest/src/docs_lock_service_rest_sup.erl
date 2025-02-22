-module(docs_lock_service_rest_sup).

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
    UsersAPIConf = #{
        spec_path => <<"../docs/docs_lock.openapi.json">>,
        preprocess_middlewares => [docs_lock_service_rest_middlewares],
        postprocess_middlewares => [docs_lock_service_rest_middlewares],
        callback => docs_lock_service_rest_callbacks,
        port => 8080
    },
    UsersChildSpec = {
        public_api_server,
        {erf, start_link, [UsersAPIConf]},
        permanent,
        5000,
        worker,
        [erf]
    },
    {ok, {Flags, [UsersChildSpec]}}.
