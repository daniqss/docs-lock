-module(docs_lock_db).

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

%%% DIALYZER OPTIONS
-dialyzer({nowarn_function, [set_id/2]}).

%%%-----------------------------------------------------------------------------
%%% USER CALLBACKS
%%%-----------------------------------------------------------------------------
-callback create_user(ID, RealName, GitUsername) -> Return when
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
%%% SKILL CALLBACKS
%%%-----------------------------------------------------------------------------
-callback create_skill(ID, SkillName, Participants) -> Return when
    ID :: binary(),
    SkillName :: binary(),
    Participants :: [binary()],
    Return :: {ok, Skill} | {error, Reason},
    Skill :: docs_lock:skill(),
    Reason :: term().

-callback delete_skill(SkillId) -> Return when
    SkillId :: binary(),
    Return :: ok | {error, Reason},
    Reason :: term().

-callback get_skill(SkillId) -> Return when
    SkillId :: binary(),
    Return :: {ok, Skill} | {error, Reason},
    Skill :: docs_lock:skill(),
    Reason :: term().

-callback get_skills(Filters) -> Return when
    Filters :: map(),
    Return :: {ok, [Skill]} | {error, Reason},
    Skill :: docs_lock:skill(),
    Reason :: term().

%%%-----------------------------------------------------------------------------
%%% SECTION CALLBACKS
%%%-----------------------------------------------------------------------------
-callback create_section(ID, SectionName, SkillId, Participants) -> Return when
    ID :: binary(),
    SectionName :: binary(),
    SkillId :: binary(),
    Participants :: [binary()],
    Return :: {ok, Section} | {error, Reason},
    Section :: docs_lock:section(),
    Reason :: term().

-callback delete_section(SectionId) -> Return when
    SectionId :: binary(),
    Return :: ok | {error, Reason},
    Reason :: term().

-callback get_section(SectionId) -> Return when
    SectionId :: binary(),
    Return :: {ok, Section} | {error, Reason},
    Section :: docs_lock:section(),
    Reason :: term().

-callback get_sections(Filters) -> Return when
    Filters :: map(),
    Return :: {ok, [Section]} | {error, Reason},
    Section :: docs_lock:section(),
    Reason :: term().

%%%-----------------------------------------------------------------------------
%%% NOTE CALLBACKS
%%%-----------------------------------------------------------------------------
-callback create_note(ID, UserId, SectionId, Content) -> Return when
    ID :: binary(),
    UserId :: binary(),
    SectionId :: binary(),
    Content :: binary(),
    Return :: {ok, Note} | {error, Reason},
    Note :: docs_lock:note(),
    Reason :: term().

-callback delete_note(NoteId) -> Return when
    NoteId :: binary(),
    Return :: ok | {error, Reason},
    Reason :: term().

-callback get_note(NoteId) -> Return when
    NoteId :: binary(),
    Return :: {ok, Note} | {error, Reason},
    Note :: docs_lock:note(),
    Reason :: term().

-callback get_notes(Filters) -> Return when
    Filters :: map(),
    Return :: {ok, [Note]} | {error, Reason},
    Note :: docs_lock:note(),
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
%%% SKILL EXPORTS
%%%-----------------------------------------------------------------------------
-spec create_skill(SkillName, Participants) -> Return when
    SkillName :: binary(),
    Participants :: [binary()],
    Return :: {ok, Skill} | {error, Reason},
    Skill :: docs_lock:skill(),
    Reason :: term().
create_skill(SkillName, Participants) -> 
    ID = set_id(Participants, get_skills),
    Mod = docs_lock_conf:dbms(),
    Mod:create_skill(ID, SkillName, Participants).

-spec delete_skill(SkillId) -> Return when
    SkillId :: binary(),
    Return :: ok | {error, Reason},
    Reason :: term().
delete_skill(SkillId) ->
    Mod = docs_lock_conf:dbms(),
    Mod:delete_skill(SkillId).

-spec get_skill(SkillId) -> Return when
    SkillId :: binary(),
    Return :: {ok, Skill} | {error, Reason},
    Skill :: docs_lock:skill(),
    Reason :: term().
get_skill(SkillId) ->
    Mod = docs_lock_conf:dbms(),
    Mod:get_skill(SkillId).

-spec get_skills(Filters) -> Return when
    Filters :: map(),
    Return :: {ok, [Skill]} | {error, Reason},
    Skill :: docs_lock:skill(),
    Reason :: term().
get_skills(Filters) ->
    Mod = docs_lock_conf:dbms(),
    Mod:get_skills(Filters).

%%%-----------------------------------------------------------------------------
%%% SECTION EXPORTS
%%%-----------------------------------------------------------------------------
-spec create_section(SectionName, SkillId, Participants) -> Return when
    SectionName :: binary(),
    SkillId :: binary(),
    Participants :: [binary()],
    Return :: {ok, Section} | {error, Reason},
    Section :: docs_lock:section(),
    Reason :: term().
create_section(SectionName, SkillId, Participants) -> 
    ID = set_id(Participants, get_sections),
    Mod = docs_lock_conf:dbms(),
    Mod:create_section(ID, SectionName, SkillId, Participants).

-spec delete_section(SectionId) -> Return when
    SectionId :: binary(),
    Return :: ok | {error, Reason},
    Reason :: term().
delete_section(SectionId) ->
    Mod = docs_lock_conf:dbms(),
    Mod:delete_section(SectionId).

-spec get_section(SectionId) -> Return when
    SectionId :: binary(),
    Return :: {ok, Section} | {error, Reason},
    Section :: docs_lock:section(),
    Reason :: term().
get_section(SectionId) ->
    Mod = docs_lock_conf:dbms(),
    Mod:get_section(SectionId).

-spec get_sections(Filters) -> Return when
    Filters :: map(),
    Return :: {ok, [Section]} | {error, Reason},
    Section :: docs_lock:section(),
    Reason :: term().
get_sections(Filters) ->
    Mod = docs_lock_conf:dbms(),
    Mod:get_sections(Filters).

%%%-----------------------------------------------------------------------------
%%% NOTE EXPORTS
%%%-----------------------------------------------------------------------------
-spec create_note(UserId, SectionId, Content) -> Return when
    UserId :: binary(),
    SectionId :: binary(),
    Content :: binary(),
    Return :: {ok, Note} | {error, Reason},
    Note :: docs_lock:note(),
    Reason :: term().
create_note(UserId, SectionId, Content) -> 
    ID = set_id(UserId, get_notes),
    Mod = docs_lock_conf:dbms(),
    Mod:create_note(ID, UserId, SectionId, Content).

-spec delete_note(NoteId) -> Return when
    NoteId :: binary(),
    Return :: ok | {error, Reason},
    Reason :: term().
delete_note(NoteId) ->
    Mod = docs_lock_conf:dbms(),
    Mod:delete_note(NoteId).

-spec get_note(NoteId) -> Return when
    NoteId :: binary(),
    Return :: {ok, Note} | {error, Reason},
    Note :: docs_lock:note(),
    Reason :: term().
get_note(NoteId) ->
    Mod = docs_lock_conf:dbms(),
    Mod:get_note(NoteId).

-spec get_notes(Filters) -> Return when
    Filters :: map(),
    Return :: {ok, [Note]} | {error, Reason},
    Note :: docs_lock:note(),
    Reason :: term().
get_notes(Filters) ->
    Mod = docs_lock_conf:dbms(),
    Mod:get_notes(Filters).

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
