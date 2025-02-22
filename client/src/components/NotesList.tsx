interface SectionNotesProps {
    selectedSection: { name: string; notes: string[] } | null;
  }
  
  const SectionNotes: React.FC<SectionNotesProps> = ({ selectedSection }) => {
    return (
      <main className="w-4/5 p-4 overflow-auto h-full">
        <h2 className="text-lg font-bold mb-4">
          {selectedSection
            ? `Notas de ${selectedSection.name}`
            : "Selecciona una sección"}
        </h2>
        <div className="overflow-auto h-[85vh] space-y-2 pr-2">
          {selectedSection ? (
            selectedSection.notes.map((note, index) => (
              <div key={index} className="p-3 bg-gray-200 rounded">
                {note}
              </div>
            ))
          ) : (
            <p className="text-gray-500">
              Haz clic en una sección para ver sus notas.
            </p>
          )}
        </div>
      </main>
    );
  };
  
  export default SectionNotes;
  