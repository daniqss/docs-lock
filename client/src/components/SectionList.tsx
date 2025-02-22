import { useState } from "react";
import { skillSection } from "../types";

interface SkillBoxProps {
  sectionList: skillSection[];
  handleClick: (section: skillSection) => void;
}

export default function SectionList({
  sectionList,
  handleClick,
}: SkillBoxProps) {
  const [selectedSection, setSelectedSection] = useState<skillSection | null>(
    null
  );

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
