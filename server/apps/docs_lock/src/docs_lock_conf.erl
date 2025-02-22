-module(docs_lock_conf).

%%% EXTERNAL EXPORTS
-export([
    dbms/0,
    dbms/1
]).

%%%-----------------------------------------------------------------------------
%%% EXTERNAL EXPORTS
%%%-----------------------------------------------------------------------------
dbms() ->
    application:get_env(docs_lock, dbms, docs_lock_db_mongo).

dbms(Val) ->
    application:set_env(docs_lock, dbms, Val).
