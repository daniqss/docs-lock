import { useState } from "react";
import Header from "../components/Header";
import SectionList from "../components/SectionList";
import SectionNotes from "../components/NotesList";
import { useParams } from "react-router";
import "./HomePage.css";
import Footer from "../components/Footer";
<<<<<<< HEAD
// import useFetch from "../hooks/useFetch";
import { skillSection } from "../types";
import { sections } from "../mocks";
import ProfileIcon from "../components/icons/ProfileIcon";

function SkillPage() {
  const [selectedSection, setSelectedSection] = useState<skillSection | null>(null);
=======
import { useGetSections } from "../api/hooks/sections.hooks";
import { Section } from "../api/__generated__";

function SkillPage() {
  const [selectedSection, setSelectedSection] = useState<Section | null>(null);
>>>>>>> dfb73eb24ec19f08e3ac4e07d005079e90fe288a
  const { skill } = useParams<{ skill: string }>();
  const [text, setText] = useState("");

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

  const wantedSections = sections.filter((section) => section.skill === skill);

  return (
    <>
      <Header />
      <main className="mt-20 mb-12 mx-4 flex-1">
        <h1 className="w-full text-3xl font-bold text-center mt-0 mb-8 bg-white text-red-500 py-4 rounded-lg">
          {skill}
        </h1>
<<<<<<< HEAD
  
=======

>>>>>>> dfb73eb24ec19f08e3ac4e07d005079e90fe288a
        {/* main container */}
        <section className="flex flex-1 gap-4">
          {/* left column */}
          <section className="w-1/3 bg-white p-4 rounded-lg items-center min-h-100 overflow-y-auto">
            <SectionList
              sectionList={sections}
              handleClick={(section) => {
                setSelectedSection(section);
              }}
            />
          </section>
  
          {/* right column */}
<<<<<<< HEAD
          <section className="w-2/3 bg-secondary p-4 rounded-lg items-center overflow-y-auto">
            <SectionNotes selectedSection={selectedSection} />
=======
          <section className="w-2/3 bg-secondary p-4 rounded-lg items-center min-h-100 overflow-y-auto">
            {isSectionLoading && !sectionError && selectedSection !== null && (
              <SectionNotes selectedSection={selectedSection} />
            )}
>>>>>>> dfb73eb24ec19f08e3ac4e07d005079e90fe288a
          </section>
        </section>
  
        {/* Input and Send Button */}
        <div className="flex items-center gap-2 p-3 border border-gray-300 rounded-lg mt-4">
          <input
            type="text"
            value={text}
            onChange={(e) => setText(e.target.value)}
            placeholder="Escribe algo..."
            className="flex-1 p-2 border border-gray-300 rounded-md outline-none focus:ring-2 focus:ring-blue-500"
          />
          <button 
            onClick={handleSend} 
            className="p-2 bg-blue-500 text-white rounded-md hover:bg-blue-600 transition"
          >
            <ProfileIcon className="w-8 h-8" />
          </button>
        </div>
      </main>
      <Footer />
    </>
  );
}  

export default SkillPage;
