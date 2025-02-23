import { useContext, useState } from "react";
import { SessionContext, SessionContextType } from "../context/Session";
import { useNavigate } from "react-router-dom";
import { useGetUsers } from "../api/hooks/users.hooks";
import BoardingBox from "../components/Boarding";
import Footer from "../components/Footer";
import AppIcon from "../components/icons/AppIcon"; // Importamos el icono

function Login() {
  const { setUser } = useContext(SessionContext) as SessionContextType;
  const navigate = useNavigate();
  const [name, setName] = useState("");
  const [github, setGithub] = useState("");
  const [error, setError] = useState("");
  const {
    data: users,
    error: userError,
    isLoading: isUserLoading,
  } = useGetUsers();

  const handleSubmit = (e: React.FormEvent<HTMLFormElement>) => {
    e.preventDefault();
    if (name === "" || github === "") {
      setError("Todos los campos son obligatorios");
      return;
    }
    setError("");

    while (true) {
      if (isUserLoading) continue;

      if (!users || userError) {
        setError("Error fetching users");
        return;
      }

      console.log(users);
      const user = users.find((u) => u.gitUsername === github);
      if (!user) {
        setError("User not found");
        return;
      }

      if (!sessionStorage.getItem("user"))
        sessionStorage.setItem("user", JSON.stringify(user));
      setUser(user);
      break;
    }

    navigate("/home");
  };

  return (
    <>
      {/* Contenedor del Título */}
      <div className="relative w-full flex justify-center">
        <h1 className="absolute top-[50%] mt-5 text-[6vw] md:text-[5vw] font-bold text-gray-900 flex items-center gap-3 z-50">
          <AppIcon className="w-[10vw] h-[10vw] max-w-[80px] max-h-[80px]" /> docs.lock
        </h1>
      </div>
  
      {/* Contenedor Principal */}
      <div className="flex items-center rounded justify-center min-h-screen bg-secondary p-4">
        <div className="flex flex-wrap justify-center gap-8 w-full max-w-[90vw]">
          
          {/* Sección de BoardingBox */}
          <section className="bg-primary p-8 rounded-lg shadow-lg w-[50%] min-w-[280px] max-w-[500px] mt-[5vh] mx-4">
            <BoardingBox />
          </section>
          
          {/* Sección de Login */}
          <section className="bg-primary p-8 rounded-lg shadow-lg w-[40%] min-w-[250px] max-w-[400px] mt-[5vh] mx-4">
            <h2 className="text-[1.8vw] md:text-[1.5vw] lg:text-2xl text-black font-bold text-center mb-6">
              Login
            </h2>
            <form onSubmit={handleSubmit}>
              <div className="mb-4">
                <label htmlFor="name" className="block text-sm font-semibold text-gray-700">
                  Name
                </label>
                <input
                  type="text"
                  id="name"
                  value={name}
                  onChange={(e) => setName(e.target.value)}
                  className="w-full p-3 mt-2 border border-gray-300 rounded-lg focus:outline-none focus:ring-2 focus:ring-blue-500 bg-input text-black"
                />
              </div>
              <div className="mb-4">
                <label htmlFor="username" className="block text-sm font-semibold text-gray-700">
                  Github Username
                </label>
                <input
                  type="text"
                  id="username"
                  value={github}
                  onChange={(e) => setGithub(e.target.value)}
                  className="w-full p-3 mt-2 border border-gray-300 rounded-lg focus:outline-none focus:ring-2 focus:ring-blue-500 bg-input text-black"
                />
              </div>
              {error && <div className="text-red-500 text-sm ">{error}</div>}
              <button
                type="submit"
                className="w-full bg-blue-500 text-white py-3 mt-4 rounded-lg hover:bg-blue-600 focus:outline-none focus:ring-2 focus:ring-blue-400 bg-button"
              >
                Login
              </button>
            </form>
          </section>
  
        </div>
      </div>
  
      <Footer />
    </>
  );
  
}

export default Login;
