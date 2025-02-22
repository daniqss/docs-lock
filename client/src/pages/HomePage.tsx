import "./HomePage.css";
import { useState } from "react";
import Header from "../components/Header";
import SkillBox from "../components/SkillBox";
import Footer from "../components/Footer";

function HomePage() {
  const [skills] = useState([
    "React",
    "JavaScript",
    "TypeScript",
    "Node.js",
    "CSS",
    "HTML",
    "Tailwind",
    "Redux",
    "Next.js",
    "GraphQL",
  ]);

  return (
    <>
      <div className="styles.container">
        <Header />
        <main className="mt-12 mb-12">
        <div className="p-4 grid grid-cols-4 gap-4">
          {skills.map((skill, index) => (
            <SkillBox key={index} name={skill} />
          ))}
        </div>
        </main>
        <Footer />
      </div>
    </>
  );
}

export default HomePage;
