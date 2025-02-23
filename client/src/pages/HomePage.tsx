import "./HomePage.css";
import { useNavigate } from "react-router-dom";
import Header from "../components/Header";
import SkillBox from "../components/SkillBox";
import Footer from "../components/Footer";
import { useGetSkills } from "../api/hooks/skills.hook";
import { Skill } from "../api/__generated__";

function HomePage() {
  const navigate = useNavigate();
  const {
    data,
    error: skillsError,
    isLoading: skillsIsLoading,
  } = useGetSkills();

  const skills = Array.isArray(data) ? data : [];

  return (
    <div className="flex flex-col min-h-screen">
      <Header />
      <main className="flex-1 flex flex-col items-center">
        <section className="p-4 grid gap-8 mt-16 mb-12 w-full grid-cols-1 sm:grid-cols-2 md:grid-cols-3 lg:grid-cols-4">
          {!skillsIsLoading && !skillsError && skills.length > 0 ? (
            skills.map((skill: Skill, index: number) => (
              <SkillBox key={index} skill={skill} />
            ))
          ) : (
            <p>No hay skills disponibles.</p>
          )}
        </section>
        <button
          onClick={() => navigate("/create-skill")}
          className="fixed bottom-10 right-10 bg-blue-500 text-white p-4 rounded-full text-2xl shadow-lg hover:bg-blue-600"
        >
          +
        </button>
      </main>
      <Footer />
    </div>
  );
}

export default HomePage;
