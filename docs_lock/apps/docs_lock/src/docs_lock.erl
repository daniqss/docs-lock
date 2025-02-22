-module(docs_lock).

%%% BEHAVIOURS
-behaviour(application).

%%% START/STOP EXPORTS
-export([
    start/2,
    stop/1
]).

%%% USER EXPORTS
-export([
    create_user/2,
    delete_user/1,
    get_user/1,
    get_users/1
]).

%%% TYPES
-type user() :: #{
    real_name := binary(),
    git_username := binary()
}.

%%% EXPORT TYPES
-export_type([
    user/0
]).

%%%-----------------------------------------------------------------------------
%%% START/STOP EXPORTS
%%%-----------------------------------------------------------------------------
start(_StartType, _StartArgs) ->
    docs_lock_sup:start_link().

stop(_St) ->
    ok.

%%%-----------------------------------------------------------------------------
%%% USER EXPORTS
%%%-----------------------------------------------------------------------------
-spec create_user(RealName, GitUsername) -> Return when
    RealName :: binary(),
    GitUsername :: binary(),
    Return :: {ok, User} | {error, Reason},
    User :: user(),
    Reason :: term().
create_user(RealName, GitUsername) -> 
    docs_lock_db:create_user(GitUsername, RealName).

-spec delete_user(UserId) -> Return when
    UserId :: binary(),
    Return :: ok | {error, Reason},
    Reason :: term().
delete_user(UserId) ->
    docs_lock_db:delete_user(UserId).

-spec get_user(UserId) -> Return when
    UserId :: binary(),
    Return :: {ok, User} | {error, Reason},
    User :: user(),
    Reason :: term().
get_user(UserId) ->
    docs_lock_db:get_user(UserId).

-spec get_users(Filters) -> Return when
    Filters :: map(),
    Return :: {ok, [User]} | {error, Reason},
    User :: user(),
    Reason :: term().
get_users(Filters) ->
    docs_lock_db:get_users(Filters).

