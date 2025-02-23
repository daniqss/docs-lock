import { Note } from "../api/__generated__";

type SectionNotesProps = {
  selectedSection: Section;
  notes: Note[];
  getUserNameAndGitHub: (userId: string) => { realName: string; gitUsername: string };
};

function SectionNotes({ selectedSection, notes, getUserNameAndGitHub }: SectionNotesProps) {
  return (
    <>
      <h2 className="text-lg font-bold mb-4 text-black">
        {selectedSection
          ? `Notas de ${selectedSection.sectionName}`
          : "Selecciona una sección"}
      </h2>
      <div className="space-y-2 mx-20">
        {notes.length > 0 ? (
          <NotesList notes={notes} getUserNameAndGitHub={getUserNameAndGitHub} /> 
        ) : (
          <p className="text-gray-500">
            Haz clic en una sección para ver sus notas.
          </p>
        )}
      </div>
    </>
  );
}

function NotesList({ notes, getUserNameAndGitHub }: { notes: Note[]; getUserNameAndGitHub: (userId: string) => { realName: string; gitUsername: string } }) {
  return notes.map((note, index) => {
    const { realName, gitUsername } = getUserNameAndGitHub(note.userId);

    return (
      <article key={index} className="p-3 bg-primary rounded text-black h-fit">
        <p className="text-sm text-gray-600">
          {realName} (@{gitUsername})
        </p>
        <p>{note.content}</p>
      </article>
    );
  });
}

export default SectionNotes;
