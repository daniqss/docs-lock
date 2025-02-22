import { useContext, useState } from "react";
import { SessionContext, SessionContextType } from "../context/Session";
import { useNavigate } from "react-router-dom";
import { useGetUsers } from "../api/hooks/users.hooks";
import BoardingBox from '../components/Boarding';
import Footer from '../components/Footer';

function Login() {
  const { setUser } = useContext(SessionContext) as SessionContextType;
  const navigate = useNavigate();
  const [name, setName] = useState("");
  const [surname, setSurname] = useState("");
  const [github, setGithub] = useState("");
  const [error, setError] = useState("");
  const {
    data: users,
    error: userError,
    isLoading: isUserLoading,
  } = useGetUsers();

  const handleSubmit = (e: React.FormEvent<HTMLFormElement>) => {
    e.preventDefault();
    if (name === "" || surname === "" || github === "") {
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
      const user = users.find((u) => u.gitUsername === github);
      if (!user) {
        setError("User not found");
        return;
      }

      // if there's no user in session storage, set it
      if (!sessionStorage.getItem("user"))
        sessionStorage.setItem("user", JSON.stringify(user));
      setUser(user);
      break;
    }

    navigate("/home");
  };

  return (
    <>
    <div className="items-center justify-center rounded-lg min-h-screen bg-secondary flex gap-4 p-4">
  <div className="flex h-full gap-8">
    <section className="bg-primary p-8 rounded-lg shadow-lg w-150 mx-6">
      <BoardingBox />
    </section>
    <section className="bg-primary p-8 rounded-lg shadow-lg w-80 mx-6">
        <h2 className="text-2xl text-black font-bold text-center mb-6">Login</h2>
        <form onSubmit={handleSubmit}>
          <div className="mb-4">
            <label
              htmlFor="name"
              className="block text-sm font-semibold text-gray-700"
            >
              Name
            </label>
            <input
              type="name"
              id="name"
              value={name}
              onChange={(e) => setName(e.target.value)}
              className="w-full p-3 mt-2 border border-gray-300 rounded-lg focus:outline-none focus:ring-2 focus:ring-blue-500 bg-input text-black"
            />
          </div>
          <div className="mb-4">
            <label
              htmlFor="surname"
              className="block text-sm font-semibold text-gray-700"
            >
              Surname
            </label>
            <input
              type="surname"
              id="surname"
              value={surname}
              onChange={(e) => setSurname(e.target.value)}
              className="w-full p-3 mt-2 border border-gray-300 rounded-lg focus:outline-none focus:ring-2 focus:ring-blue-500 bg-input text-black"
            />
          </div>
          <div className="mb-4">
            <label
              htmlFor="username"
              className="block text-sm font-semibold text-gray-700"
            >
              Username
            </label>
            <input
              type="username"
              id="username"
              value={github}
              onChange={(e) => setGithub(e.target.value)}
              className="w-full p-3 mt-2 border border-gray-300 rounded-lg focus:outline-none focus:ring-2 focus:ring-blue-500 bg-input text-black"
            />
          </div>
          {error && <div className="text-red-500 text-sm mb-4">{error}</div>}
          <button
            type="submit"
            className="w-full bg-blue-500 text-white py-3 rounded-lg hover:bg-blue-600 focus:outline-none focus:ring-2 focus:ring-blue-400 bg-button"
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
