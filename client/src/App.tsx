import "./App.css";
import Header from "./components/Header";
import SkillBox from "./components/SkillBox";
import Footer from "./components/Footer";

function App() {
  const skills = [
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
  ];

  return (
    <>
      <Header>docs.lock</Header>
      {/* <SkillBox /> */}
      <div className="p-4 grid grid-cols-4 gap-4">
        {skills.map((skill, index) => (
          <SkillBox key={index} name={skill} />
        ))}
      </div>
      <Footer />
    </>
  );
}

export default App;
