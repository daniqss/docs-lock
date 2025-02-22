import { useQuery, useMutation } from "@tanstack/react-query";
import {
  createNote,
  deleteNote,
  getNote,
  getNotes,
} from "../client/services/notes";
import { CreateNoteRequest, Note } from "../__generated__/types.gen";

export const useGetNotes = () => {
  return useQuery<Note[], Error>({
    queryKey: ["notes"],
    queryFn: getNotes,
  });
};

export const useGetNote = (noteId: string) => {
  return useQuery<Note, Error>({
    queryKey: ["note", noteId],
    queryFn: async () => await getNote(noteId),
  });
};

export const useCreateNote = () => {
  return useMutation({
    mutationFn: (newNote: CreateNoteRequest) => {
      return Promise.resolve(createNote(newNote));
    },
  });
};

export const useDeleteNote = () => {
  return useMutation({
    mutationFn: (noteId: string) => {
      return Promise.resolve(deleteNote(noteId));
    },
  });
};
