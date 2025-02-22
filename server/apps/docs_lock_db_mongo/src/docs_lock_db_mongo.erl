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

%%% SKILL EXPORTS
-export([
    create_skill/3,
    delete_skill/1,
    get_skill/1,
    get_skills/1
]).

%%% SECTION EXPORTS
-export([
    create_section/4,
    delete_section/1,
    get_section/1,
    get_sections/1
]).

%%% NOTE EXPORTS
-export([
    create_note/4,
    delete_note/1,
    get_note/1,
    get_notes/1
]).

%%% MACROS
-define(USERS_COLLECTION, <<"users">>).
-define(SKILLS_COLLECTION, <<"skills">>).
-define(NOTES_COLLECTION, <<"notes">>).
-define(SECTIONS_COLLECTION, <<"sections">>).

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
%%% SKILL EXPORTS
%%%-----------------------------------------------------------------------------
-spec create_skill(ID, SkillName, Participants) -> Return when
    ID :: binary(),
    SkillName :: binary(),
    Participants :: [binary()],
    Return :: {ok, Skill} | {error, Reason},
    Skill :: docs_lock:skill(),
    Reason :: term().
create_skill(ID, SkillName, Participants) -> 
    CreateSkillReq = #{
        <<"_id">> => ID,
        <<"skillName">> => SkillName,
        <<"participants">> => Participants
    },
    create(?SKILLS_COLLECTION, CreateSkillReq).

-spec delete_skill(SkillId) -> Return when
    SkillId :: binary(),
    Return :: ok | {error, Reason},
    Reason :: term().
delete_skill(SkillId) ->
    delete(?SKILLS_COLLECTION, SkillId).

-spec get_skill(SkillId) -> Return when
    SkillId :: binary(),
    Return :: {ok, Skill} | {error, Reason},
    Skill :: docs_lock:skill(),
    Reason :: term().
get_skill(SkillId) ->
    get_by_id(?SKILLS_COLLECTION, SkillId).

-spec get_skills(Filters) -> Return when
    Filters :: map(),
    Return :: {ok, [Skill]} | {error, Reason},
    Skill :: docs_lock:skill(),
    Reason :: term().
get_skills(Filters) ->
    get_all(?SKILLS_COLLECTION, Filters).

%%%-----------------------------------------------------------------------------
%%% SECTION EXPORTS
%%%-----------------------------------------------------------------------------
-spec create_section(ID, SectionName, SkillId, Participants) -> Return when
    ID :: binary(),
    SectionName :: binary(),
    SkillId :: binary(),
    Participants :: [binary()],
    Return :: {ok, Section} | {error, Reason},
    Section :: docs_lock:section(),
    Reason :: term().
create_section(ID, SectionName, SkillId, Participants) -> 
    CreateSectionReq = #{
        <<"_id">> => ID,
        <<"sectionName">> => SectionName,
        <<"skillId">> => SkillId,
        <<"participants">> => Participants
    },
    create(?SECTIONS_COLLECTION, CreateSectionReq).

-spec delete_section(SectionId) -> Return when
    SectionId :: binary(),
    Return :: ok | {error, Reason},
    Reason :: term().
delete_section(SectionId) ->
    delete(?SECTIONS_COLLECTION, SectionId).

-spec get_section(SectionId) -> Return when
    SectionId :: binary(),
    Return :: {ok, Section} | {error, Reason},
    Section :: docs_lock:section(),
    Reason :: term().
get_section(SectionId) ->
    get_by_id(?SECTIONS_COLLECTION, SectionId).

-spec get_sections(Filters) -> Return when
    Filters :: map(),
    Return :: {ok, [Section]} | {error, Reason},
    Section :: docs_lock:section(),
    Reason :: term().
get_sections(Filters) ->
    get_all(?SECTIONS_COLLECTION, Filters).

%%%-----------------------------------------------------------------------------
%%% NOTE EXPORTS
%%%-----------------------------------------------------------------------------
-spec create_note(ID, UserId, SectionId, Content) -> Return when
    ID :: binary(),
    SectionId :: binary(),
    UserId :: binary(),
    Content :: binary(),
    Return :: {ok, Note} | {error, Reason},
    Note :: docs_lock:note(),
    Reason :: term().
create_note(ID, UserId, SectionId, Content) -> 
    CreateNoteReq = #{
        <<"_id">> => ID,
        <<"userId">> => UserId,
        <<"sectionId">> => SectionId,
        <<"content">> => Content
    },
    create(?NOTES_COLLECTION, CreateNoteReq).

-spec delete_note(NoteId) -> Return when
    NoteId :: binary(),
    Return :: ok | {error, Reason},
    Reason :: term().
delete_note(NoteId) ->
    delete(?NOTES_COLLECTION, NoteId).

-spec get_note(NoteId) -> Return when
    NoteId :: binary(),
    Return :: {ok, Note} | {error, Reason},
    Note :: docs_lock:note(),
    Reason :: term().
get_note(NoteId) ->
    get_by_id(?NOTES_COLLECTION, NoteId).

-spec get_notes(Filters) -> Return when
    Filters :: map(),
    Return :: {ok, [Note]} | {error, Reason},
    Note :: docs_lock:note(),
    Reason :: term().
get_notes(Filters) ->
    get_all(?NOTES_COLLECTION, Filters).

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
