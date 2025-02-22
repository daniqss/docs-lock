import { useState } from "react";
import { Skill } from "../api/__generated__";

interface SkillBoxProps {
  sectionList: Skill[];
  handleClick: (section: Skill) => void;
}

export default function SectionList({
  sectionList,
  handleClick,
}: SkillBoxProps) {
  const [selectedSection, setSelectedSection] = useState<Skill | null>(null);

  return (
    <div className="text-center bg-white">
      {/* Título */}
      <h2 className="text-lg text-black font-bold mb-4">Lista de Secciones</h2>

      {/* Lista de secciones */}
      <ul className="space-y-2 ">
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
