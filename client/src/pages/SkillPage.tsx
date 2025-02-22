import { useState } from "react";
import Header from "../components/Header";
import SectionList from "../components/SectionList";
import SectionNotes from "../components/NotesList";
import { useParams } from "react-router";
import "./HomePage.css";
import Footer from "../components/Footer";
// import useFetch from "../hooks/useFetch";
import { skillSection } from "../types";
import { sections } from "../mocks";
import ProfileIcon from "../components/icons/ProfileIcon";

function SkillPage() {
  const [selectedSection, setSelectedSection] = useState<skillSection | null>(null);
  const { skill } = useParams<{ skill: string }>();
  const [text, setText] = useState("");

  const handleSend = async () => {
    if (!text.trim()) return;
    console.log("Enviando a la base de datos:", text);
    setText("");
  };

  // example of useFetch
  // const { data, loading, error } = useFetch(
  //   `https://pokeapi.co/api/v2/pokemon/ditto`
  // );
  // console.log(data, loading, error);

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
              sectionList={sections}
              handleClick={(section) => {
                setSelectedSection(section);
              }}
            />
          </section>
  
          {/* right column */}
          <section className="w-2/3 bg-secondary p-4 rounded-lg items-center overflow-y-auto">
            <SectionNotes selectedSection={selectedSection} />
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
