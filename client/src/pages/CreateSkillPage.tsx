import { useState } from "react";
import { useNavigate } from "react-router-dom";
import { useCreateSkill } from "../api/hooks/skills.hook";
import { CreateSkillRequest } from "../api/__generated__";

function CreateSkillPage() {
  const navigate = useNavigate();
  const createSkillMutation = useCreateSkill();

  const [skillName, setSkillName] = useState("");
  const [participants] = useState<Array<string | undefined>>([]);
  const [error, setError] = useState("");

  const handleSubmit = (e: React.FormEvent) => {
    e.preventDefault();
    if (!skillName.trim()) {
      setError("El nombre de la skill es obligatorio.");
      return;
    }
    setError("");

    const newSkill: CreateSkillRequest = { skillName, participants };
    createSkillMutation.mutate(newSkill, {
      onSuccess: () => navigate("/home"), // Ahora redirige a /home
      onError: () => setError("Error al crear la skill. Inténtalo de nuevo."),
    });
  };

  return (
    <div className="flex flex-col items-center justify-center max-w-[80vh] max-h-[60vh] w-full h-full bg-white rounded absolute top-1/2 left-1/2 transform -translate-x-1/2 -translate-y-1/2 p-4">
<h1 className="text-3xl font-bold mb-6 text-black">Añadir Nueva Skill</h1>
      
      <form onSubmit={handleSubmit} className="bg-gray-200 p-6 rounded-lg shadow-lg w-full max-w-md">
        <div className="mb-4">
          <label className="block text-sm font-semibold text-gray-700 mb-4">Nombre de la Skill</label>
          <input
            type="text"
            value={skillName}
            onChange={(e) => setSkillName(e.target.value)}
            className="w-full p-3 border border-gray-300 rounded-lg focus:ring-2 focus:ring-blue-500 focus:outline-none bg-input text-black"
            placeholder="Ej: Programación en Python"
            required
          />
        </div>

        {error && <p className="text-red-500 text-sm mb-4">{error}</p>}

        <button
          type="submit"
          className="w-full bg-button text-white py-3 rounded-lg hover:bg-blue-600 transition-all"
        >
          Añadir Skill
        </button>
      </form>
    </div>
  );
}

export default CreateSkillPage;
