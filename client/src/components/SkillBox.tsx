interface SkillBoxProps {
  name: string;
}

export default function SkillBox({ name }: SkillBoxProps) {
  return (
    <div className="w-64 h-64 flex items-center justify-center bg-gray-200 border border-gray-400 rounded-xl shadow-xl">
      <h1 className="text-2xl font-bold text-gray-700">{name}</h1>
    </div>
  );
}
