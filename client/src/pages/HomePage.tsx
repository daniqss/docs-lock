import "./HomePage.css";
import { useState } from "react";
import Header from "../components/Header";
import SkillBox from "../components/SkillBox";
import Footer from "../components/Footer";
import { skills as skillsMock } from "../mocks";

function HomePage() {
  const [skills] = useState(skillsMock);

  return (
    <div className="flex flex-col min-h-screen">
      <Header />
      <main className="flex-1 flex">
        <div
          className="p-4 grid gap-4 mt-16 mb-12 w-full grid-cols-1 sm:grid-cols-2 md:grid-cols-3 lg:grid-cols-4"
        >
          {skills.map((skill, index) => (
            <SkillBox key={index} name={skill} />
          ))}
        </div>
      </main>
      <Footer />
    </div>
  );
}


export default HomePage;
