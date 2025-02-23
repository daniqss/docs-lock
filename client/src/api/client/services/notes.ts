import { Note, CreateNoteRequest } from "../../__generated__";
import client from "../client";

export const createNote = (createNoteRequest: CreateNoteRequest) =>
  client.post("/notes", createNoteRequest).then((response) => response.data);

export const deleteNote = (noteId: string) => {
  client.delete(`/notes/${noteId}`).then((response) => response.data);
};

export const getNote = (noteId: string) =>
  client.get<Note>(`/notes/${noteId}`).then((response) => response.data);

export const getNotes = async (sectionId?: string): Promise<Note[]> => {
  const url = sectionId ? `/notes?sectionId=${sectionId}` : "/notes";
  const response = await client.get<Note[]>(url);
  return response.data;
};
