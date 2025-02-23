import "./HomePage.css";
import Header from "../components/Header";
import SkillBox from "../components/SkillBox";
import Footer from "../components/Footer";
import { useGetSkills } from "../api/hooks/skills.hook";
import { Skill } from "../api/__generated__";

function HomePage() {
  const {
    data: skills = [] as Skill[],
    error: skillsError,
    isLoading: skillsIsLoading,
  } = useGetSkills();

  if (!skillsIsLoading && !skillsError) {
    console.log(skills);
    return (
      <div className="flex flex-col min-h-screen">
        <Header />
        <main className="flex-1 flex">
          <section className="p-4 grid gap-8 mt-16 mb-12 w-full grid-cols-1 sm:grid-cols-2 md:grid-cols-3 lg:grid-cols-4">
            {skills.length > 0 ? (
              skills.map((skill, index) => {
                return (
                  <SkillBox key={index} name={skill.name ?? "Sin nombre"} />
                );
              })
            ) : (
              <p>No hay skills disponibles.</p>
            )}
          </section>
        </main>
        <Footer />
      </div>
    );
  }
}

export default HomePage;
