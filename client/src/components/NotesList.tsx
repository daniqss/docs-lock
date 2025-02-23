import { Note, Section } from "../api/__generated__";
import { UseQueryResult } from "@tanstack/react-query";

type SectionNotesProps = {
  selectedSection: Section;
  getNotes: UseQueryResult<Note[], Error>;
  notes: Note[];
};

function SectionNotes({ selectedSection, getNotes, notes }: SectionNotesProps) {
  const { isLoading: notesIsLoading, error: notesError } = getNotes;

  return (
    <>
      <h2 className="text-lg font-bold mb-4 text-black">
        {selectedSection
          ? `Notas de ${selectedSection.sectionName}`
          : "Selecciona una sección"}
      </h2>
      <div className="space-y-2 mx-20">
        {!notesIsLoading && !notesError ? (
          <NotesList notes={notes} />
        ) : (
          <p className="text-gray-500">
            Haz clic en una sección para ver sus notas.
          </p>
        )}
      </div>
    </>
  );
}

function NotesList({ notes }: { notes: Note[] }) {
  return notes.map((note, index) => (
    <article key={index} className="p-3 bg-primary rounded text-black h-fit">
      <h3 className="text-md font-semibold">{note.userId}</h3>
      <p>{note.content}</p>
    </article>
  ));
}

export default SectionNotes;
