-module(docs_lock_service_rest_callbacks).

%%% USER EXPORTS
-export([
    create_user/1,
    get_users/1,
    get_user/1,
    delete_user/1
]).

%%%-----------------------------------------------------------------------------
%%% USER EXPORTS
%%%-----------------------------------------------------------------------------
create_user(#{body := Body} = _Request) ->
    RealName = maps:get(<<"realName">>, Body, undefined), 
    GitUsername = maps:get(<<"gitUsername">>, Body, undefined),
    {ok, User} = docs_lock:create_user(RealName, GitUsername), 
    {201, [], User}.

get_users(_Request) ->
    case docs_lock:get_users(#{}) of
        {error, not_found} ->
            {404, [], #{
                <<"message">> =>
                    <<"Error not found">>
            }};
        {ok, UserList} ->
            {200, [], [User || User <- UserList]}
    end.

get_user(#{path_parameters := PathParameters} = _Request) ->
    UserId = proplists:get_value(<<"userId">>, PathParameters),
    case docs_lock:get_user(UserId) of
        {error, not_found} ->
            {404, [], #{
                <<"message">> =>
                    <<"User not found">>
            }};
        {ok, User} ->
            {200, [], User}
    end.

delete_user(#{path_parameters := PathParameters} = _Request) ->
    UserId = proplists:get_value(<<"userId">>, PathParameters),
    case docs_lock:delete_user(UserId) of
        {error, not_found} ->
            {404, [], #{
                <<"message">> =>
                    <<"User not found">>
            }};
        ok ->
            {204, [], #{}}
    end.

%%%-----------------------------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%%-----------------------------------------------------------------------------
