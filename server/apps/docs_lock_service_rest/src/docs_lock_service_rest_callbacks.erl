-module(docs_lock_service_rest_callbacks).

%%% USER EXPORTS
-export([
    create_user/1,
    get_users/1,
    get_user/1,
    delete_user/1
]).

%%% SKILL EXPORTS
-export([
    create_skill/1,
    get_skills/1,
    get_skill/1,
    delete_skill/1
]).

%%% SECTION EXPORTS
-export([
    create_section/1,
    get_sections/1,
    get_section/1,
    delete_section/1
]).

%%% NOTE EXPORTS
-export([
    create_note/1,
    get_notes/1,
    get_note/1,
    delete_note/1
]).

%%%-----------------------------------------------------------------------------
%%% USER EXPORTS
%%%-----------------------------------------------------------------------------
create_user(#{body := Body} = _Request) ->
    RealName = maps:get(<<"realName">>, Body, undefined), 
    GitUsername = maps:get(<<"gitUsername">>, Body, undefined),
    {ok, User} = docs_lock:create_user(RealName, GitUsername), 
    {201, [], User}.

get_users(#{query_parameters := QueryParameters} = _Request) ->
    Filters = maps:from_list(QueryParameters),
    case docs_lock:get_users(Filters) of
        {error, _Reason} ->
            {500, [], #{
                <<"message">> =>
                    <<"Internal Server Error">>
            }};
        {ok, UserList} ->
            {200, [], UserList}
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
%%% SKILL EXPORTS
%%%-----------------------------------------------------------------------------
create_skill(#{body := Body} = _Request) ->
    SkillName = maps:get(<<"skillName">>, Body, undefined), 
    Participants = maps:get(<<"participants">>, Body, []),
    {ok, Skill} = docs_lock:create_skill(SkillName, Participants), 
    {201, [], Skill}.

get_skills(#{query_parameters := QueryParameters} = _Request) ->
    Filters = maps:from_list(QueryParameters),
    case docs_lock:get_skills(Filters) of
        {error, _Reason} ->
            {500, [], #{
                <<"message">> =>
                    <<"Internal Server Error">>
            }};
        {ok, SkillList} ->
            {200, [], SkillList}
    end.

get_skill(#{path_parameters := PathParameters} = _Request) ->
    SkillId = proplists:get_value(<<"skillId">>, PathParameters),
    case docs_lock:get_skill(SkillId) of
        {error, not_found} ->
            {404, [], #{
                <<"message">> =>
                    <<"Skill not found">>
            }};
        {ok, Skill} ->
            {200, [], Skill}
    end.

delete_skill(#{path_parameters := PathParameters} = _Request) ->
    SkillId = proplists:get_value(<<"skillId">>, PathParameters),
    case docs_lock:delete_skill(SkillId) of
        {error, not_found} ->
            {404, [], #{
                <<"message">> =>
                    <<"Skill not found">>
            }};
        ok ->
            {204, [], #{}}
    end.

%%%-----------------------------------------------------------------------------
%%% SECTION EXPORTS
%%%-----------------------------------------------------------------------------
create_section(#{body := Body} = _Request) ->
    SectionName = maps:get(<<"sectionName">>, Body, undefined),
    SkillId = maps:get(<<"skillId">>, Body, undefined),
    Participants = maps:get(<<"participants">>, Body, []),
    {ok, Section} = docs_lock:create_section(SectionName, SkillId, Participants),
    {201, [], Section}.

get_sections(#{query_parameters := QueryParameters} = _Request) ->
    Filters = maps:from_list(QueryParameters),
    case docs_lock:get_sections(Filters) of
        {error, _Reason} ->
            {500, [], #{
                <<"message">> =>
                    <<"Internal Server Error">>
            }};
        {ok, SectionList} ->
            {200, [], SectionList}
    end.

get_section(#{path_parameters := PathParameters} = _Request) ->
    SectionId = proplists:get_value(<<"sectionId">>, PathParameters),
    case docs_lock:get_section(SectionId) of
        {error, not_found} ->
            {404, [], #{
                <<"message">> =>
                    <<"Section not found">>
            }};
        {ok, Section} ->
            {200, [], Section}
    end.

delete_section(#{path_parameters := PathParameters} = _Request) ->
    SectionId = proplists:get_value(<<"sectionId">>, PathParameters),
    case docs_lock:delete_section(SectionId) of
        {error, not_found} ->
            {404, [], #{
                <<"message">> =>
                    <<"Section not found">>
            }};
        ok ->
            {204, [], #{}}
    end.

%%%-----------------------------------------------------------------------------
%%% NOTE EXPORTS
%%%-----------------------------------------------------------------------------
create_note(#{body := Body} = _Request) ->
    UserId = maps:get(<<"userId">>, Body, undefined),
    SectionId = maps:get(<<"sectionId">>, Body, undefined),
    Content = maps:get(<<"content">>, Body, undefined),
    {ok, Note} = docs_lock:create_note(UserId, SectionId, Content),
    {201, [], Note}.

get_notes(#{query_parameters := QueryParameters} = _Request) ->
    Filters = maps:from_list(QueryParameters),
    case docs_lock:get_notes(Filters) of
        {error, _Reason} ->
            {500, [], #{
                <<"message">> =>
                    <<"Internal Server Error">>
            }};
        {ok, NoteList} ->
            {200, [], NoteList}
    end.

get_note(#{path_parameters := PathParameters} = _Request) ->
    NoteId = proplists:get_value(<<"noteId">>, PathParameters),
    case docs_lock:get_note(NoteId) of
        {error, not_found} ->
            {404, [], #{
                <<"message">> =>
                    <<"Note not found">>
            }};
        {ok, Note} ->
            {200, [], Note}
    end.

delete_note(#{path_parameters := PathParameters} = _Request) ->
    NoteId = proplists:get_value(<<"noteId">>, PathParameters),
    case docs_lock:delete_note(NoteId) of
        {error, not_found} ->
            {404, [], #{
                <<"message">> =>
                    <<"Note not found">>
            }};
        ok ->
            {204, [], #{}}
    end.
