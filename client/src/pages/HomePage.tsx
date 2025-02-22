import "./HomePage.css";
import { useState } from "react";
import Header from "../components/Header";
import SkillBox from "../components/SkillBox";
import Footer from "../components/Footer";
import { skills as skillsMock } from "../mocks";

function HomePage() {
  const [skills] = useState(skillsMock);

  return (
    <>
      <Header />
      <main>
        <div className="p-4 grid grid-cols-4 gap-4">
          {skills.map((skill, index) => (
            <SkillBox key={index} name={skill} />
          ))}
        </div>
      </main>
      <Footer />
    </>
  );
}

export default HomePage;
