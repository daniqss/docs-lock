import { Link } from "react-router";
import { Skill } from "../api/__generated__";
import { useContext } from "react";
import { SessionContext, SessionContextType } from "../context/Session";

interface SkillBoxProps {
  skill: Skill;
}

export default function SkillBox({ skill }: SkillBoxProps) {
  const { setSelectedSkill } = useContext(SessionContext) as SessionContextType;

  return (
    <Link
      onClick={() => setSelectedSkill(skill)}
      to={`/skills/${skill.skillName}`}
      className="w-full aspect-square flex items-center justify-center bg-primary border-gray-400 rounded-xl shadow-xl overflow-hidden"
    >
      <h1 className="flex font-bold text-center px-4 text-[clamp(0.8rem,4vw,2.5rem)] mx-2">
        {skill.skillName}
      </h1>
    </Link>
  );
}
