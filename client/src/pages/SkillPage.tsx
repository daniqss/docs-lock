import { useState } from "react";
import Header from "../components/Header";
import SectionList from "../components/SectionList";
import SectionNotes from "../components/NotesList";
import { useParams } from "react-router";
import "./HomePage.css";
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
      <main className="mt-6 mb-12">
      <h1 className="w-full text-3xl font-bold text-center mt-1 mb-4 bg-gray-200 py-2 rounded-lg">
       {skill}
      </h1>

        {/* Contenedor principal de las columnas */}
        <div className="flex flex-1 gap-4 px-4">
          {/* Columna izquierda */}
          <div className="w-1/3 bg-gray-100 p-4 rounded-lg">
            <SectionList sectionList={sections} handleClick={handleClick} />
          </div>

          {/* Columna derecha */}
          <div className="w-2/3 bg-gray-50 p-4 rounded-lg">
            <SectionNotes selectedSection={selectedSection} />
          </div>
        </div>
      </main>
      <Footer />
    </>
  );
}

export default SkillPage;
