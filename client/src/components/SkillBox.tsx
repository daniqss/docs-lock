import { Link } from "react-router";

interface SkillBoxProps {
  name: string;
}

export default function SkillBox({ name }: SkillBoxProps) {
  return (
    <Link
      to={`/skills/${name}`}
      className="w-full aspect-square flex items-center justify-center bg-primary border-gray-400 rounded-xl shadow-xl overflow-hidden"
    >
      <h1 className="flex font-bold text-center px-4 text-[clamp(0.8rem,4vw,2.5rem)] mx-2">
        {name}
      </h1>
    </Link>
  );
}


