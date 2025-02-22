import { useState } from "react";

// Define la interfaz `Section` con las propiedades correctas
interface Section {
  name: string;
  notes: string[];
}

interface SkillBoxProps {
  sectionList: Section[]; // Se espera que `sectionList` sea un array de objetos con `name` y `notes`
  handleClick: (section: Section) => void; // `handleClick` toma una sección como argumento
}


export default function SectionList({ sectionList, handleClick }: SkillBoxProps) {
    const [selectedSection, setSelectedSection] = useState<Section | null>(null); // Estado para la sección seleccionada
  
    return (
        <div className="flex h-screen">
        {/* Columna izquierda (Lista de secciones) */}
        <aside className="w-64 bg-gray-100 p-4 border-r h-full overflow-y-auto">
          <h2 className="text-lg font-bold mb-4">Lista de Secciones</h2>
          <ul className="space-y-2 pr-2">
            {sectionList.map((section) => (
              <li
                key={section.name}
                className={`p-2 rounded cursor-pointer ${
                  selectedSection?.name === section.name
                    ? "bg-blue-500 text-white"
                    : "bg-white hover:bg-gray-200"
                }`}
                onClick={() => {
                  setSelectedSection(section);
                  handleClick(section);
                }}
              >
                {section.name}
              </li>
            ))}
          </ul>
        </aside>
      </div>
    );
  }
