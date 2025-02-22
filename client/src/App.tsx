import "./App.css";
import { useState } from "react";
import Header from "./components/Header";
import SkillBox from "./components/SkillBox";
import Footer from "./components/Footer";
import SkillPage from "./pages/SkillPage";

function App() {
  // eslint-disable-next-line @typescript-eslint/no-unused-vars
  const [skills, setSkills] = useState([
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
  const [isSkillSelected, setIsSkillSelected] = useState(false);
  const [selectedSkill, setSelectedSkill] = useState("");

  return (
    <>
      {!isSkillSelected ? (
        <main>
          <Header />
          <div className="p-4 grid grid-cols-4 gap-4">
            {skills.map((skill, index) => (
              <SkillBox
                key={index}
                name={skill}
                handleClick={() => {
                  setSelectedSkill(skill);
                  setIsSkillSelected(!isSkillSelected);
                }}
              />
            ))}
          </div>
          <Footer />
        </main>
      ) : (
        <SkillPage skill={selectedSkill} />
      )}
    </>
  );
}

export default App;
