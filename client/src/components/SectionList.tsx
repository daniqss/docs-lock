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
      <div className="text-center bg-white">
        {/* Título */}
        <h2 className="text-lg text-black font-bold mb-4">Lista de Secciones</h2>
  
        {/* Lista de secciones */}
        <ul className="overflow-auto h-[85vh] space-y-2 ">
          {sectionList.map((section) => (
            <li
              key={section.name}
              className={`p-2 rounded cursor-pointer ${
                selectedSection?.name === section.name
                  ? "bg-red-500 text-white"
                  : "bg-primary text-black hover:bg-gray-200"
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
      </div>
  );
}  