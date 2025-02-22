import { useState } from "react";
import Header from "../components/Header";
import SectionList from "../components/SectionList";
import SectionNotes from "../components/NotesList";
import { useParams } from "react-router";
import Footer from "../components/Footer";
import useFetch from "../hooks/useFetch";

// Define the Section type
type Section = {
  name: string;
  notes: string[];
};

// Datos de ejemplo
const sections: Section[] = [
  { name: "Hooks", notes: ["useState para estado", "useEffect para efectos"] },
  {
    name: "Context",
    notes: ["useContext para estado global", "Context.Provider"],
  },
  {
    name: "ES6+",
    notes: ["Arrow functions", "Destructuring", "Spread/rest operator"],
  },
  { name: "Async", notes: ["Promises", "Async/Await"] },
  { name: "Performance", notes: ["React.memo", "useMemo", "useCallback"] },
  { name: "Testing", notes: ["Jest", "React Testing Library", "Cypress"] },
];

function SkillPage() {
  const [selectedSection, setSelectedSection] = useState<Section | null>(null);
  const { skill } = useParams<{ skill: string }>();

  // example of useFetch
  const { data, loading, error } = useFetch(
    `https://pokeapi.co/api/v2/pokemon/ditto`
  );
  console.log(data, loading, error);

  const handleClick = (section: Section) => {
    setSelectedSection(section); // Actualiza la sección seleccionada
  };

  return (
    <>
      <Header />
      <main>
        <h1 className="text-3xl font-bold text-center my-4">{skill}</h1>

        <div className="flex h-screen">
          <SectionList sectionList={sections} handleClick={handleClick} />
          <SectionNotes selectedSection={selectedSection} />
        </div>
      </main>
      <Footer />
    </>
  );
}

export default SkillPage;
