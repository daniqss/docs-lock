import { useState } from "react";
import Header from "../components/Header";
import SectionList from "../components/SectionList";
import SectionNotes from "../components/NotesList";
import { useParams } from "react-router";
import "./HomePage.css";
import Footer from "../components/Footer";
// import useFetch from "../hooks/useFetch";
import { skillSection } from "../types";
import { sections } from "../mocks";

function SkillPage() {
  const [selectedSection, setSelectedSection] = useState<skillSection | null>(
    null
  );
  const { skill } = useParams<{ skill: string }>();

  // example of useFetch
  // const { data, loading, error } = useFetch(
  //   `https://pokeapi.co/api/v2/pokemon/ditto`
  // );
  // console.log(data, loading, error);

  return (
    <>
      <Header />
      <main className="mt-20 mb-12 mx-4 flex-1">
      <h1 className="w-full text-3xl font-bold text-center mt-0 mb-8 bg-white text-red-500 py-4 rounded-lg">
        {skill}
      </h1>


        {/* main container */}
        <section className="flex flex-1 gap-4">
          {/* left column */}
          <section className="w-1/3 bg-white p-4 rounded-lg items-center">
            <SectionList
              sectionList={sections}
              handleClick={(section) => {
                setSelectedSection(section);
              }}
            />
          </section>

          {/* right column */}
          <section className="w-2/3 bg-secondary p-4 rounded-lg items-center">
            <SectionNotes selectedSection={selectedSection} />
          </section>
        </section>
      </main>
      <Footer />
    </>
  );
}

export default SkillPage;
