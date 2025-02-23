import { useContext, useState } from "react";
import Header from "../components/Header";
import SectionList from "../components/SectionList";
import SectionNotes from "../components/NotesList";
import { useParams } from "react-router";
import "./HomePage.css";
import Footer from "../components/Footer";
import { Section } from "../api/__generated__";
import { useGetSections } from "../api/hooks/sections.hooks";
import PaperIcon from "../components/icons/PaperIcon";
import { SessionContext, SessionContextType } from "../context/Session";

function SkillPage() {
  const [selectedSection, setSelectedSection] = useState<Section | null>(null);
  const { skill } = useParams<{ skill: string }>();
  const [text, setText] = useState("");
  const { selectedSkill } = useContext(SessionContext) as SessionContextType;

  const handleSend = async () => {
    if (!text.trim()) return;
    console.log("Enviando a la base de datos:", text);
    setText("");
  };

  const {
    data: sections = [],
    error: sectionError,
    isLoading: isSectionLoading,
  } = useGetSections();

  const wantedSections = sections.filter((section) => {
    console.log(selectedSkill);
    // console.log(section.skillId, " == ", selectedSkill.id);
    return section.skillId === selectedSkill._id;
  });

  console.log(sections);

  return (
    <>
      <Header />
      <main className="mt-20 mb-12 mx-4 flex-1">
        <h1 className="w-full text-3xl font-bold text-center mt-0 mb-8 bg-white text-red-500 py-4 rounded-lg">
          {skill}
        </h1>
        {/* main container */}
        <section className="flex flex-1 gap-4">
          {/* left column */}
          <section className="w-1/3 bg-white p-4 rounded-lg items-center min-h-100 overflow-y-auto">
            <SectionList
              sectionList={wantedSections}
              handleClick={(section) => {
                setSelectedSection(section);
              }}
            />
          </section>
          <section className="w-2/3 bg-secondary p-4 rounded-lg items-center flex-col min-h-100">
            {/* right column */}
            <section className="items-center min-h-70 overflow-y-auto">
              {isSectionLoading &&
                !sectionError &&
                selectedSection !== null && (
                  <SectionNotes selectedSection={selectedSection} />
                )}
            </section>
            {/* Input and Send Button */}
            <div className="flex items-center gap-2 p-3 mt-6">
              <input
                type="text"
                value={text}
                onChange={(e) => setText(e.target.value)}
                placeholder="Escribe algo..."
                className="flex-1 h-11 p-2 border text-black bg-gray-100 border-gray-500 rounded-md outline-none focus:ring-2 focus:ring-blue-500"
              />
              <button
                onClick={handleSend}
                className="p-2 bg-blue-500 text-white rounded-md hover:bg-blue-600 transition"
              >
                <PaperIcon className="w-6 h-6 " />
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
