import { useState } from "react";
import { Section, Skill } from "../api/__generated__";

interface SectionBoxProps {
  sectionList: Section[];
  handleClick: (section: Skill) => void;
}

export default function SectionList({
  sectionList,
  handleClick,
}: SectionBoxProps) {
  const [selectedSection, setSelectedSection] = useState<Section | null>(null);

  return (
    <div className="text-center bg-white">
      {/* Título */}
      <h2 className="text-lg text-black font-bold mb-4">Lista de Secciones</h2>

      {/* Lista de secciones */}
      <ul className="space-y-2 ">
        {sectionList.map((section) => (
          <li
            key={section._id}
            className={`p-2 rounded cursor-pointer ${
              selectedSection?.sectionName === section.sectionName
                ? "bg-red-500 text-white"
                : "bg-primary text-black hover:bg-gray-200"
            }`}
            onClick={() => {
              setSelectedSection(section);
              handleClick(section);
            }}
          >
            {section.sectionName}
          </li>
        ))}
      </ul>
    </div>
  );
}
