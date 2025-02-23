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
    <div className="flex flex-col items-center justify-center min-h-screen p-4 bg-secondary">
      <h1 className="text-3xl font-bold mb-6 text-black">Crear Nueva Skill</h1>
      
      <form onSubmit={handleSubmit} className="bg-primary p-6 rounded-lg shadow-lg w-full max-w-md">
        <div className="mb-4">
          <label className="block text-sm font-semibold text-gray-700 mb-1">Nombre de la Skill</label>
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
          Crear Skill
        </button>
      </form>
    </div>
  );
}

export default CreateSkillPage;
