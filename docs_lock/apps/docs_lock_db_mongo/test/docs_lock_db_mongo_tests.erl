-module(docs_lock_db_mongo_tests).

%%% INCLUDE FILES
-include_lib("eunit/include/eunit.hrl").

%%%-----------------------------------------------------------------------------
%%% TESTS DESCRIPTIONS
%%%-----------------------------------------------------------------------------
user_test_() ->
    {"Ensure that the user functions works properly",
        [fun start/0, fun test_user_/0]}.

skill_test_() ->
    {"Ensure that the skills functions works properly",
        [fun start/0, fun test_skill_/0]}.

section_test_() ->
    {"Ensure that the sections functions works properly",
        [fun start/0, fun test_section_/0]}.

note_test_() ->
    {"Ensure that the notes functions works properly",
        [fun start/0, fun test_note_/0]}.
        

%%%-----------------------------------------------------------------------------
%%% SETUP FUNCTION
%%%-----------------------------------------------------------------------------
start() ->
    application:ensure_all_started(docs_lock_db_mongo),
    application:ensure_all_started(docs_lock).

%%%-----------------------------------------------------------------------------
%%% TESTS
%%%-----------------------------------------------------------------------------
test_user_() ->
    RealName = <<"John Doe">>,
    GitUsername = <<"johndoe">>,
    UserId = <<"12345">>,
    User = #{<<"_id">> => UserId, <<"realName">> => RealName, <<"gitUsername">> => GitUsername},

    ?assertEqual({ok, User}, docs_lock_db_mongo:create_user(UserId, RealName, GitUsername)),
    ?assertEqual({ok, User}, docs_lock_db_mongo:get_user(UserId)),
    ?assertEqual({ok, [User]}, docs_lock_db_mongo:get_users(#{})),
    ?assertEqual(ok, docs_lock_db_mongo:delete_user(UserId)),
    ?assertEqual({ok, []}, docs_lock_db_mongo:get_users(#{})).

test_skill_() ->
    SkillName = <<"Eunit">>,
    Participants = [<<"user1">>, <<"user2">>],
    SkillId = <<"12345">>,
    Skill = #{<<"_id">> => SkillId, <<"skillName">> => SkillName, <<"participants">> => Participants},

    ?assertEqual({ok, Skill}, docs_lock_db_mongo:create_skill(SkillId, SkillName, Participants)),
    ?assertEqual({ok, Skill}, docs_lock_db_mongo:get_skill(SkillId)),
    ?assertEqual({ok, [Skill]}, docs_lock_db_mongo:get_skills(#{})),
    ?assertEqual(ok, docs_lock_db_mongo:delete_skill(SkillId)),
    ?assertEqual({ok, []}, docs_lock_db_mongo:get_skills(#{})).

test_section_() ->
    SectionName = <<"Eunit in Rebar">>,
    SkillId = <<"12345">>,
    Participants = [<<"user1">>, <<"user2">>],
    SectionId = <<"67890">>,
    Section = #{<<"_id">> => SectionId, <<"sectionName">> => SectionName, <<"skillId">> => SkillId, <<"participants">> => Participants},

    ?assertEqual({ok, Section}, docs_lock_db_mongo:create_section(SectionId, SectionName, SkillId, Participants)),
    ?assertEqual({ok, Section}, docs_lock_db_mongo:get_section(SectionId)),
    ?assertEqual({ok, [Section]}, docs_lock_db_mongo:get_sections(#{})),
    ?assertEqual(ok, docs_lock_db_mongo:delete_section(SectionId)),
    ?assertEqual({ok, []}, docs_lock_db_mongo:get_sections(#{})).

test_note_() ->
    NoteId = <<"12345">>,
    UserId = <<"user123">>,
    SectionId = <<"67890">>,
    Content = <<"This is a note">>,
    Note = #{<<"_id">> => NoteId, <<"userId">> => UserId, <<"sectionId">> => SectionId, <<"content">> => Content},
    
    ?assertEqual({ok, Note}, docs_lock_db_mongo:create_note(NoteId, UserId, SectionId, Content)),
    ?assertEqual({ok, Note}, docs_lock_db_mongo:get_note(NoteId)),
    ?assertEqual({ok, [Note]}, docs_lock_db_mongo:get_notes(#{})),
    ?assertEqual(ok, docs_lock_db_mongo:delete_note(NoteId)),
    ?assertEqual({ok, []}, docs_lock_db_mongo:get_notes(#{})).
