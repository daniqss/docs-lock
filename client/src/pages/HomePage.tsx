import "./HomePage.css";
import Header from "../components/Header";
import SkillBox from "../components/SkillBox";
import Footer from "../components/Footer";
import { useGetSkills } from "../api/hooks/skills.hook";
import { Skill } from "../api/__generated__";

function HomePage() {
  const { data: skills = [], error, isLoading } = useGetSkills();

  return (
    <div className="flex flex-col min-h-screen">
      <Header />
      <main className="flex-1 flex">
        {isLoading ? (
          <p className="p-4">Cargando skills...</p>
        ) : error ? (
          <p className="p-4 text-red-500">
            Error al cargar skills: {error.message}
          </p>
        ) : (
          <SkillBoxList skills={skills} />
        )}
      </main>
      <Footer />
    </div>
  );
}

function SkillBoxList({ skills }: { skills: Skill[] }) {
  return (
    <section className="p-4 grid gap-8 mt-16 mb-12 w-full grid-cols-1 sm:grid-cols-2 md:grid-cols-3 lg:grid-cols-4">
      {skills.length > 0 ? (
        skills.map((skill) => (
          <SkillBox key={skill.id} name={skill.name ?? "Sin nombre"} />
        ))
      ) : (
        <p>No hay skills disponibles.</p>
      )}
    </section>
  );
}

export default HomePage;
