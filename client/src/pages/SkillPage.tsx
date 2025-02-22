import { useState } from "react";
import Header from "../components/Header";

// Definimos el tipo para una sección
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

// Definimos el tipo para las props del componente
type SkillPageProps = {
  skill: string;
};

function SkillPage({ skill }: SkillPageProps) {
  const [selectedSection, setSelectedSection] = useState<Section | null>(null); // Inicializa sin selección

  return (
    <>
      <Header />
      <div className="flex h-screen">
        {/* Columna izquierda (Lista de secciones) */}
        <aside className="w-1/5 bg-gray-100 p-4 border-r overflow-auto h-full">
          <h2 className="text-lg font-bold mb-4">{skill}</h2>
          <ul className="space-y-2 overflow-auto h-[85vh] pr-2">
            {sections.map((section) => (
              <li
                key={section.name}
                className={`p-2 rounded cursor-pointer ${
                  selectedSection?.name === section.name
                    ? "bg-blue-500 text-white"
                    : "bg-white hover:bg-gray-200"
                }`}
                onClick={() => setSelectedSection(section)}
              >
                {section.name}
              </li>
            ))}
          </ul>
        </aside>

        {/* Columna derecha (Notas de la sección seleccionada) */}
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
      </div>
    </>
  );
}

export default SkillPage;
