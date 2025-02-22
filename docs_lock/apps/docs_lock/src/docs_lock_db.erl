-module(docs_lock_db).

%%% USER EXPORTS
-export([
    create_user/2,
    delete_user/1,
    get_user/1,
    get_users/1
]).

%%% DIALYZER OPTIONS
-dialyzer({nowarn_function, [set_id/2]}).

%%%-----------------------------------------------------------------------------
%%% USER CALLBACKS
%%%-----------------------------------------------------------------------------
-callback create_user(ID,RealName, GitUsername) -> Return when
    ID :: binary(),
    RealName :: binary(),
    GitUsername :: binary(),
    Return :: {ok, User} | {error, Reason},
    User :: docs_lock:user(),
    Reason :: term().

-callback delete_user(UserId) -> Return when
    UserId :: binary(),
    Return :: ok | {error, Reason},
    Reason :: term().

-callback get_user(UserId) -> Return when
    UserId :: binary(),
    Return :: {ok, User} | {error, Reason},
    User :: docs_lock:user(),
    Reason :: term().

-callback get_users(Filters) -> Return when
    Filters :: map(),
    Return :: {ok, [User]} | {error, Reason},
    User :: docs_lock:user(),
    Reason :: term().

%%%-----------------------------------------------------------------------------
%%% USER EXPORTS
%%%-----------------------------------------------------------------------------
-spec create_user(RealName, GitUsername) -> Return when
    RealName :: binary(),
    GitUsername :: binary(),
    Return :: {ok, User} | {error, Reason},
    User :: docs_lock:user(),
    Reason :: term().
create_user(RealName, GitUsername) -> 
    ID = set_id(GitUsername, get_users),
    Mod = docs_lock_conf:dbms(),
    Mod:create_user(ID, RealName, GitUsername).

-spec delete_user(UserId) -> Return when
    UserId :: binary(),
    Return :: ok | {error, Reason},
    Reason :: term().
delete_user(UserId) ->
    Mod = docs_lock_conf:dbms(),
    Mod:delete_user(UserId).

-spec get_user(UserId) -> Return when
    UserId :: binary(),
    Return :: {ok, User} | {error, Reason},
    User :: docs_lock:user(),
    Reason :: term().
get_user(UserId) ->
    Mod = docs_lock_conf:dbms(),
    Mod:get_user(UserId).

-spec get_users(Filters) -> Return when
    Filters :: map(),
    Return :: {ok, [User]} | {error, Reason},
    User :: docs_lock:user(),
    Reason :: term().
get_users(Filters) ->
    Mod = docs_lock_conf:dbms(),
    Mod:get_users(Filters).

%%%-----------------------------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%%-----------------------------------------------------------------------------
set_id(Parameter, Fun) ->
    case apply(?MODULE, Fun, [#{<<"gitUsername">> => Parameter}]) of
        {ok, []} ->
            ID = nuid:nuid2();
        {ok, [Result]} ->
            ID = maps:get(<<"_id">>, Result)
    end,
    ID.
