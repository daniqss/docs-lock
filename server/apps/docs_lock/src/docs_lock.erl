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

%%% SKILL EXPORTS
-export([
    create_skill/2,
    delete_skill/1,
    get_skill/1,
    get_skills/1
]).

%%% SECTION EXPORTS
-export([
    create_section/3,
    delete_section/1,
    get_section/1,
    get_sections/1
]).

%%% NOTE EXPORTS
-export([
    create_note/3,
    delete_note/1,
    get_note/1,
    get_notes/1
]).

%%% TYPES
-type user() :: #{}.
% #{
%     <<"realName">> := binary(),
%     <<"gitUsername">> := binary()
% }.

-type skill() :: #{}.
% #{
%     <<"skillName">> := binary(),
%     <<"participants">> := [binary()]
% }.

-type section() :: #{}.
% #{
%     <<"sectionName">> := binary(),
%     <<"skillId">> := binary(),
%     <<"participants">> := [binary()]
% }.

-type note() :: #{}.
% #{
%     <<"userId">> := binary(),
%     <<"sectionId">> := binary(),
%     <<"content">> := binary()
% }.

%%% EXPORT TYPES
-export_type([
    user/0,
    skill/0,
    section/0,
    note/0
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

%%%-----------------------------------------------------------------------------
%%% SKILL EXPORTS
%%%-----------------------------------------------------------------------------
-spec create_skill(SkillName, Participants) -> Return when
    SkillName :: binary(),
    Participants :: [binary()], 
    Return :: {ok, Skill} | {error, Reason},
    Skill :: skill(),
    Reason :: term().
create_skill(SkillName, Participants) -> 
    docs_lock_db:create_skill(SkillName, Participants).

-spec delete_skill(SkillId) -> Return when
    SkillId :: binary(),
    Return :: ok | {error, Reason},
    Reason :: term().
delete_skill(SkillId) ->
    docs_lock_db:delete_skill(SkillId).

-spec get_skill(SkillId) -> Return when
    SkillId :: binary(),
    Return :: {ok, Skill} | {error, Reason},
    Skill :: skill(),
    Reason :: term().
get_skill(SkillId) ->
    docs_lock_db:get_skill(SkillId).

-spec get_skills(Filters) -> Return when
    Filters :: map(),
    Return :: {ok, [Skill]} | {error, Reason},
    Skill :: user(),
    Reason :: term().
get_skills(Filters) ->
    docs_lock_db:get_skills(Filters).

%%%-----------------------------------------------------------------------------
%%% SECTION EXPORTS
%%%-----------------------------------------------------------------------------
-spec create_section(SectionName, SkillId, Participants) -> Return when
    SectionName :: binary(),
    SkillId :: binary(),
    Participants :: [binary()],
    Return :: {ok, Section} | {error, Reason},
    Section :: section(),
    Reason :: term().
create_section(SectionName, SkillId, Participants) -> 
    docs_lock_db:create_section(SectionName, SkillId, Participants).

-spec delete_section(SectionId) -> Return when
    SectionId :: binary(),
    Return :: ok | {error, Reason},
    Reason :: term().
delete_section(SectionId) ->
    docs_lock_db:delete_section(SectionId).

-spec get_section(SectionId) -> Return when
    SectionId :: binary(),
    Return :: {ok, Section} | {error, Reason},
    Section :: section(),
    Reason :: term().
get_section(SectionId) ->
    docs_lock_db:get_section(SectionId).

-spec get_sections(Filters) -> Return when
    Filters :: map(),
    Return :: {ok, [Section]} | {error, Reason},
    Section :: section(),
    Reason :: term().
get_sections(Filters) ->
    docs_lock_db:get_sections(Filters).

%%%-----------------------------------------------------------------------------
%%% NOTE EXPORTS
%%%-----------------------------------------------------------------------------
-spec create_note(UserId, SectionId, Content) -> Return when
    UserId :: binary(),
    SectionId :: binary(),
    Content :: binary(),
    Return :: {ok, Note} | {error, Reason},
    Note :: note(),
    Reason :: term().
create_note(UserId, SectionId, Content) -> 
    docs_lock_db:create_note(UserId, SectionId, Content).

-spec delete_note(NoteId) -> Return when
    NoteId :: binary(),
    Return :: ok | {error, Reason},
    Reason :: term().
delete_note(NoteId) ->
    docs_lock_db:delete_note(NoteId).

-spec get_note(NoteId) -> Return when
    NoteId :: binary(),
    Return :: {ok, Note} | {error, Reason},
    Note :: note(),
    Reason :: term().
get_note(NoteId) ->
    docs_lock_db:get_note(NoteId).

-spec get_notes(Filters) -> Return when
    Filters :: map(),
    Return :: {ok, [Note]} | {error, Reason},
    Note :: note(),
    Reason :: term().
get_notes(Filters) ->
    docs_lock_db:get_notes(Filters).
