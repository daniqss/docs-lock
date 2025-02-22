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
      <main className="mt-6 mb-12">
        <h1 className="w-full text-3xl font-bold text-center mt-1 mb-4 bg-gray-200 py-2 rounded-lg">
          {skill}
        </h1>

        {/* main container */}
        <section className="flex flex-1 gap-4 px-4">
          {/* left column */}
          <section className="w-1/3 bg-gray-100 p-4 rounded-lg">
            <SectionList
              sectionList={sections}
              handleClick={(section) => {
                setSelectedSection(section);
              }}
            />
          </section>

          {/* right column */}
          <section className="w-2/3 bg-gray-50 p-4 rounded-lg items-center">
            <SectionNotes selectedSection={selectedSection} />
          </section>
        </section>
      </main>
      <Footer />
    </>
  );
}

export default SkillPage;
