-module(docs_lock_service_rest_middlewares).

%%% BEHAVIOURS
-behaviour(erf_preprocess_middleware).
-behaviour(erf_postprocess_middleware).

%%% MIDDLEWARE EXPORTS
-export([
    preprocess/1,
    postprocess/2
]).

%%% MACROS
-define(CORS_HEADERS, [
    {<<"access-control-allow-methods">>, <<"GET, HEAD, OPTIONS, POST, PUT, DELETE, PATCH">>},
    {<<"access-control-allow-origin">>, <<"*">>},
    {<<"access-control-allow-headers">>,
        <<"access-control-allow-headers, authorization, origin, accept, x-requested-with, content-type, access-control-request-method, access-control-request-headers">>}
]).

%%%-----------------------------------------------------------------------------
%%% MIDDLEWARE EXPORTS
%%%-----------------------------------------------------------------------------
preprocess(
    #{  
        port :=_Port,
        path := _Path,
        host := _Host,
        body := _Body,
        query_parameters := _QueryParameters, 
        method := options
    }
) ->
    {stop, {200, ?CORS_HEADERS, #{}}};
preprocess(Request) ->
    Request.

postprocess(_Req, {PartialStatus, PartialHeaders, PartialBody}) ->
    Headers =
        lists:foldl(
            fun({HeaderKey, _HeaderValue} = Header, Acc) ->
                case lists:keyfind(HeaderKey, 1, Acc) of
                    false ->
                        [Header | Acc];
                    {HeaderKey, _HeaderValue2} ->
                        Acc
                end
            end,
            PartialHeaders,
            ?CORS_HEADERS
        ),
    {PartialStatus, Headers, PartialBody}.
