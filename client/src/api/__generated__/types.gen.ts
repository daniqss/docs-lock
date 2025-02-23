// This file is auto-generated by @hey-api/openapi-ts

export type UserId = string;

export type CreateUserRequest = {
    gitUsername: string;
    realName: string;
};

export type User = {
    id?: number;
    gitUsername?: string;
    Realname?: string;
};

/**
 * Unique identifier for a user
 */
export type SkillId = string;

export type CreateSkillRequest = {
    skillName: string;
    participants: Array<unknown>;
};

export type Skill = {
    id?: number;
    name?: string;
    participants?: {
        participants?: Array<UserId>;
    };
};

/**
 * Unique identifier for a section
 */
export type SectionId = string;

export type CreateSectionRequest = {
    sectionName?: string;
    skillId?: string;
    participants?: Array<UserId>;
};

export type Section = {
    id?: number;
    seccionName?: string;
    skill?: {
        [key: string]: unknown;
    };
    participants?: {
        participants?: Array<UserId>;
    };
};

/**
 * Unique identifier for a note
 */
export type NoteId = string;

export type CreateNoteRequest = {
    /**
     * Identifier for the user
     */
    userId: string;
    /**
     * Identifier for the section
     */
    sectionId: string;
    /**
     * The content of the note
     */
    content: string;
};

export type Note = {
    id?: number;
    /**
     * Identifier for the user
     */
    userId?: string;
    /**
     * Identifier for the section
     */
    sectionId?: string;
    /**
     * The content of the note
     */
    content?: string;
};

export type _Error = {
    code?: number;
    message?: string;
};

export type UserId2 = string;

export type SkillId2 = string;

export type SectionId2 = string;

export type NoteId2 = string;

export type GetUsersData = {
    body?: never;
    path?: never;
    query?: {
        gitUsername?: string;
        realName?: string;
    };
    url: '/users';
};

export type GetUsersErrors = {
    /**
     * Not found
     */
    404: _Error;
    /**
     * Unexpected Error
     */
    default: _Error;
};

export type GetUsersError = GetUsersErrors[keyof GetUsersErrors];

export type GetUsersResponses = {
    /**
     * OK
     */
    200: Array<User>;
};

export type GetUsersResponse = GetUsersResponses[keyof GetUsersResponses];

export type CreateUserData = {
    body: CreateUserRequest;
    path?: never;
    query?: never;
    url: '/users';
};

export type CreateUserErrors = {
    /**
     * Invalid request data
     */
    400: _Error;
    /**
     * Unexpected Error
     */
    default: _Error;
};

export type CreateUserError = CreateUserErrors[keyof CreateUserErrors];

export type CreateUserResponses = {
    /**
     * User created successfully
     */
    201: User;
};

export type CreateUserResponse = CreateUserResponses[keyof CreateUserResponses];

export type DeleteUserData = {
    body?: never;
    path: {
        userId: string;
    };
    query?: never;
    url: '/users/{userId}';
};

export type DeleteUserErrors = {
    /**
     * Not found
     */
    404: _Error;
    /**
     * Unexpected Error
     */
    default: _Error;
};

export type DeleteUserError = DeleteUserErrors[keyof DeleteUserErrors];

export type DeleteUserResponses = {
    /**
     * No content
     */
    204: void;
};

export type DeleteUserResponse = DeleteUserResponses[keyof DeleteUserResponses];

export type GetUserData = {
    body?: never;
    path: {
        userId: string;
    };
    query?: never;
    url: '/users/{userId}';
};

export type GetUserErrors = {
    /**
     * Not found
     */
    404: _Error;
    /**
     * Unexpected Error
     */
    default: _Error;
};

export type GetUserError = GetUserErrors[keyof GetUserErrors];

export type GetUserResponses = {
    /**
     * OK
     */
    200: User;
};

export type GetUserResponse = GetUserResponses[keyof GetUserResponses];

export type GetSkillsData = {
    body?: never;
    path?: never;
    query?: {
        skillName?: string;
    };
    url: '/skills';
};

export type GetSkillsErrors = {
    /**
     * Not found
     */
    404: unknown;
    /**
     * Unexpected Error
     */
    default: _Error;
};

export type GetSkillsError = GetSkillsErrors[keyof GetSkillsErrors];

export type GetSkillsResponses = {
    /**
     * OK
     */
    200: Array<Skill>;
};

export type GetSkillsResponse = GetSkillsResponses[keyof GetSkillsResponses];

export type CreateSkillData = {
    body: CreateSkillRequest;
    path?: never;
    query?: never;
    url: '/skills';
};

export type CreateSkillErrors = {
    /**
     * Invalid request data
     */
    400: _Error;
    /**
     * Unexpected Error
     */
    default: _Error;
};

export type CreateSkillError = CreateSkillErrors[keyof CreateSkillErrors];

export type CreateSkillResponses = {
    /**
     * Skill created successfully
     */
    201: Skill;
};

export type CreateSkillResponse = CreateSkillResponses[keyof CreateSkillResponses];

export type DeleteSkillData = {
    body?: never;
    path: {
        skillId: string;
    };
    query?: never;
    url: '/skills/{skillId}';
};

export type DeleteSkillErrors = {
    /**
     * Not found
     */
    404: _Error;
    /**
     * Unexpected Error
     */
    default: _Error;
};

export type DeleteSkillError = DeleteSkillErrors[keyof DeleteSkillErrors];

export type DeleteSkillResponses = {
    /**
     * No content
     */
    204: void;
};

export type DeleteSkillResponse = DeleteSkillResponses[keyof DeleteSkillResponses];

export type GetSkillData = {
    body?: never;
    path: {
        skillId: string;
    };
    query?: never;
    url: '/skills/{skillId}';
};

export type GetSkillErrors = {
    /**
     * Unexpected Error
     */
    default: _Error;
};

export type GetSkillError = GetSkillErrors[keyof GetSkillErrors];

export type GetSkillResponses = {
    /**
     * OK
     */
    200: Skill;
};

export type GetSkillResponse = GetSkillResponses[keyof GetSkillResponses];

export type GetSectionsData = {
    body?: never;
    path?: never;
    query?: {
        sectionName?: string;
        skillId?: string;
    };
    url: '/sections';
};

export type GetSectionsErrors = {
    /**
     * Unexpected Error
     */
    default: _Error;
};

export type GetSectionsError = GetSectionsErrors[keyof GetSectionsErrors];

export type GetSectionsResponses = {
    /**
     * OK
     */
    200: Array<Section>;
};

export type GetSectionsResponse = GetSectionsResponses[keyof GetSectionsResponses];

export type CreateSectionData = {
    body: CreateSectionRequest;
    path?: never;
    query?: never;
    url: '/sections';
};

export type CreateSectionErrors = {
    /**
     * Invalid request data
     */
    400: _Error;
    /**
     * Unexpected Error
     */
    default: _Error;
};

export type CreateSectionError = CreateSectionErrors[keyof CreateSectionErrors];

export type CreateSectionResponses = {
    /**
     * Section created successfully
     */
    201: Section;
};

export type CreateSectionResponse = CreateSectionResponses[keyof CreateSectionResponses];

export type DeleteSectionData = {
    body?: never;
    path: {
        sectionId: string;
    };
    query?: never;
    url: '/sections/{sectionId}';
};

export type DeleteSectionErrors = {
    /**
     * Not found
     */
    404: _Error;
    /**
     * Unexpected Error
     */
    default: _Error;
};

export type DeleteSectionError = DeleteSectionErrors[keyof DeleteSectionErrors];

export type DeleteSectionResponses = {
    /**
     * No content
     */
    204: void;
};

export type DeleteSectionResponse = DeleteSectionResponses[keyof DeleteSectionResponses];

export type GetSectionData = {
    body?: never;
    path: {
        sectionId: string;
    };
    query?: {
        userId?: string;
        sectionId?: string;
        content?: string;
    };
    url: '/sections/{sectionId}';
};

export type GetSectionErrors = {
    /**
     * Not found
     */
    404: _Error;
    /**
     * Unexpected Error
     */
    default: _Error;
};

export type GetSectionError = GetSectionErrors[keyof GetSectionErrors];

export type GetSectionResponses = {
    /**
     * OK
     */
    200: Section;
};

export type GetSectionResponse = GetSectionResponses[keyof GetSectionResponses];

export type GetNotesData = {
    body?: never;
    path?: never;
    query?: never;
    url: '/notes';
};

export type GetNotesErrors = {
    /**
     * Unexpected Error
     */
    default: _Error;
};

export type GetNotesError = GetNotesErrors[keyof GetNotesErrors];

export type GetNotesResponses = {
    /**
     * OK
     */
    200: Array<Note>;
};

export type GetNotesResponse = GetNotesResponses[keyof GetNotesResponses];

export type CreateNoteData = {
    body: CreateNoteRequest;
    path?: never;
    query?: never;
    url: '/notes';
};

export type CreateNoteErrors = {
    /**
     * Invalid request data
     */
    400: _Error;
    /**
     * Unexpected Error
     */
    default: _Error;
};

export type CreateNoteError = CreateNoteErrors[keyof CreateNoteErrors];

export type CreateNoteResponses = {
    /**
     * Note created successfully
     */
    201: Note;
};

export type CreateNoteResponse = CreateNoteResponses[keyof CreateNoteResponses];

export type DeleteNoteData = {
    body?: never;
    path: {
        noteId: string;
    };
    query?: never;
    url: '/notes/{noteId}';
};

export type DeleteNoteErrors = {
    /**
     * Not found
     */
    404: _Error;
    /**
     * Unexpected Error
     */
    default: _Error;
};

export type DeleteNoteError = DeleteNoteErrors[keyof DeleteNoteErrors];

export type DeleteNoteResponses = {
    /**
     * No content
     */
    204: void;
};

export type DeleteNoteResponse = DeleteNoteResponses[keyof DeleteNoteResponses];

export type GetNoteData = {
    body?: never;
    path: {
        noteId: string;
    };
    query?: never;
    url: '/notes/{noteId}';
};

export type GetNoteErrors = {
    /**
     * Not found
     */
    404: _Error;
    /**
     * Unexpected Error
     */
    default: _Error;
};

export type GetNoteError = GetNoteErrors[keyof GetNoteErrors];

export type GetNoteResponses = {
    /**
     * OK
     */
    200: Note;
};

export type GetNoteResponse = GetNoteResponses[keyof GetNoteResponses];

export type ClientOptions = {
    baseUrl: `${string}://${string}` | (string & {});
};