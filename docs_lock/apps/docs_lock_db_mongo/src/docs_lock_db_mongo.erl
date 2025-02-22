-module(docs_lock_db_mongo).

%%% BEHAVIOURS
-behaviour(application).
-behaviour(docs_lock_db).

%%% START/STOP EXPORTS
-export([
    start/2,
    stop/1
]).

%%% USER EXPORTS
-export([
    create_user/3,
    delete_user/1,
    get_user/1,
    get_users/1
]).

%%% MACROS
-define(USERS_COLLECTION, <<"users">>).

%%%-----------------------------------------------------------------------------
%%% START/STOP EXPORTS
%%%-----------------------------------------------------------------------------
start(_StartType, _StartArgs) ->
    docs_lock_db_mongo_sup:start_link().

stop(_St) ->
    ok.

%%%-----------------------------------------------------------------------------
%%% USER EXPORTS
%%%-----------------------------------------------------------------------------
-spec create_user(ID,RealName, GitUsername) -> Return when
    ID :: binary(),
    RealName :: binary(),
    GitUsername :: binary(),
    Return :: {ok, User} | {error, Reason},
    User :: docs_lock:user(),
    Reason :: term().
create_user(ID, RealName, GitUsername) -> 
    CreateUserReq = #{
        <<"_id">> => ID,
        <<"realName">> => RealName,
        <<"gitUsername">> => GitUsername
    },
    create(?USERS_COLLECTION, CreateUserReq).

-spec delete_user(UserId) -> Return when
    UserId :: binary(),
    Return :: ok | {error, Reason},
    Reason :: term().
delete_user(UserId) ->
    delete(?USERS_COLLECTION, UserId).

-spec get_user(UserId) -> Return when
    UserId :: binary(),
    Return :: {ok, User} | {error, Reason},
    User :: docs_lock:user(),
    Reason :: term().
get_user(UserId) ->
    get_by_id(?USERS_COLLECTION, UserId).

-spec get_users(Filters) -> Return when
    Filters :: map(),
    Return :: {ok, [User]} | {error, Reason},
    User :: docs_lock:user(),
    Reason :: term().
get_users(Filters) ->
    get_all(?USERS_COLLECTION, Filters).

%%%-----------------------------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%%-----------------------------------------------------------------------------
create(Collection, CreateReq) ->
    Connection = persistent_term:get(connection),
    case mc_worker_api:insert(Connection, Collection, CreateReq) of
        {{true, _Count}, Result} ->
            {ok, Result};
        {{false, _Map}, Result} ->
            {error, Result}
    end.

delete(Collection, ID) ->
    Connection = persistent_term:get(connection),
    case mc_worker_api:delete(Connection, Collection, #{<<"_id">> => ID}) of
        {false, _Result} ->
            {error, not_found};
        {true, _Result}  ->
            ok
    end.

get_by_id(Collection, ID) ->
    Connection = persistent_term:get(connection),
    case mc_worker_api:find_one(Connection, Collection, #{<<"_id">> => ID}) of
        undefined ->
            {error, not_found};
        Result ->
            {ok, Result}
    end.

get_all(Collection, Filters) ->
    Connection = persistent_term:get(connection),
    case mc_worker_api:find(Connection, Collection, Filters) of
        [] ->
            {ok, []};
        {ok, Cursor} ->
            consume_cursor(Cursor, [])
    end.

consume_cursor(Cursor, List) ->
    case mc_cursor:next(Cursor) of
        error ->
            {ok, List};
        {Result} ->
            consume_cursor(Cursor, [Result | List])
    end.
