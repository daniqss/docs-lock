import { useContext, useState, useEffect } from "react";
import Header from "../components/Header";
import SectionList from "../components/SectionList";
import SectionNotes from "../components/NotesList";
import { useParams } from "react-router";
import "./HomePage.css";
import Footer from "../components/Footer";
import { Note, Section } from "../api/__generated__";
import { useGetSections, useCreateSection } from "../api/hooks/sections.hooks";
import { SessionContext, SessionContextType } from "../context/Session";
import { useGetUser } from "../api/hooks/users.hooks";
import { useGetNotes, useCreateNote } from "../api/hooks/notes.hooks";
import PaperIcon from "../components/icons/PaperIcon";

function SkillPage() {
  const [selectedSection, setSelectedSection] = useState<Section | null>(null);
  const { skill } = useParams<{ skill: string }>();
  const { selectedSkill, user } = useContext(SessionContext) as SessionContextType;
  const [text, setText] = useState("");
  const [newSectionName, setNewSectionName] = useState("");


  const { data: sections = [], refetch } = useGetSections();

  const { data: notes = [], refetch: refetchNotes, isLoading: isNotesLoading } = useGetNotes(
    selectedSection ? selectedSection._id : "" 
  );

  const { mutate: createSection } = useCreateSection();
  const { mutate: createNote } = useCreateNote();

  useEffect(() => {
    if (selectedSection) {
      refetchNotes();
    }
  }, [selectedSection, refetchNotes]);

  const wantedSections = sections.filter(
    (section) => section.skillId === selectedSkill?._id
  );

  const handleSend = async () => {
    if (!text.trim() || !selectedSection) return;

    if (!user?._id) {
      console.error("El usuario no está autenticado.");
      return;
    }

    const newNote: Note = {
      content: text,
      sectionId: selectedSection._id!,
      userId: user._id!
    };

    createNote(newNote, {
      onSuccess: () => {
        setText("");
        refetchNotes(); 
      },
      onError: (error: any) => {
        console.error("Error al enviar la nota:", error);
      },
    });
  };

  const handleCreateSection = () => {
    if (!selectedSkill || !user || !newSectionName.trim()) {
      console.error("Error: Faltan datos necesarios.");
      return;
    }
    console.log("Creando sección... ", newSectionName, selectedSkill._id, user._id);

    createSection(
      {
        sectionName: newSectionName,
        skillId: selectedSkill._id!,
        participants: [user._id!],
      },
      {
        onSuccess: (newSection) => {
          console.log("Sección creada:", newSection);
          setNewSectionName("");  
          refetch(); 
        },
        onError: (error) => {
          console.error("Error creando sección:", error);
        },
      }
    );
  };

  const getUserNameAndGitHub = (userId: string) => {
    const { data: user, isLoading, error } = useGetUser(userId);
  
    if (isLoading) {
      return { realName: "Cargando...", gitUsername: "Cargando..." };
    }
  
    if (error) {
      return { realName: "Error al cargar usuario", gitUsername: "Error al cargar GitHub" };
    }
  
    if (!user) {
      return { realName: "Usuario no encontrado", gitUsername: "GitHub no disponible" };
    }
  
    return {
      realName: user.realName || "Nombre no disponible",
      gitUsername: user.gitUsername || "GitHub no disponible",
    };
  };

  return (
    <>
      <Header />
      <main className="mt-20 mb-12 mx-4 flex-1">
        <h1 className="w-full text-3xl font-bold text-center mt-0 mb-8 bg-white text-red-500 py-4 rounded-lg">
          {skill}
        </h1>

        <section className="flex flex-1  gap-4">
          <section className="w-1/3 bg-white p-4 rounded-lg bg-white min-h-[60vh] max-h-[60vh] shadow-lg">
          <section className="min-h-[40vh] max-h-[40vh] overflow-y-auto">
            {wantedSections.length === 0 ? (
              <p className="text-gray-500  text-center">No hay secciones disponibles.</p>
            ) : (
              <SectionList
                sectionList={wantedSections}
                handleClick={(section) => {
                  setSelectedSection(section);
                }}
              />
            )}
            </section>
            <div className="mb-10">
              <input
                type="text"
                value={newSectionName}
                onChange={(e) => setNewSectionName(e.target.value)}
                placeholder="Nombre de la nueva sección"
                className="w-full p-2 border text-black bg-input border-gray-500 rounded-md outline-none focus:ring-2 focus:ring-blue-500"
              />
              <button
                onClick={handleCreateSection}
                className="mt-2 w-full p-2 bg-button text-white rounded-md hover:bg-blue-600 transition"
              >
                Añadir Sección
              </button>
            </div>
          </section>

          <section className="w-2/3 bg-secondary p-4 rounded-lg flex flex-col min-h-[60vh] max-h-[60vh] shadow-lg">
            <section className="min-h-[40vh] max-h-[40vh] overflow-y-auto">
              {isNotesLoading ? (
                <p className="text-gray-500 text-center">Cargando notas...</p>
              ) : !selectedSection ? (
                <p className="text-gray-500 text-center">Selecciona una sección para ver notas.</p>
              ) : (
                <SectionNotes
                  selectedSection={selectedSection}
                  notes={notes.filter(note => note.sectionId === selectedSection._id?.toString())} 
                  getNotes={refetchNotes}
                  getUserNameAndGitHub={getUserNameAndGitHub}
                />
              )}
            </section>

            <div className="flex items-center gap-4 p-3 mt-9">
              <input
                type="text"
                value={text}
                onChange={(e) => setText(e.target.value)}
                placeholder="Escribe algo..."
                className="flex-1 h-11 p-2 border text-black bg-input border-gray-500 rounded-md outline-none focus:ring-2 focus:ring-blue-500"
              />
              <button
                onClick={handleSend}
                className="p-2 bg-button text-white rounded-md hover:bg-blue-600 transition"
              >
                <PaperIcon className="w-6 h-6" />
              </button>
            </div>
          </section>
        </section>
      </main>

      <Footer />
    </>
  );
}

export default SkillPage;
