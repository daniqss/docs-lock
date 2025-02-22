import { useContext, useState } from 'react';
import { SessionContext, SessionContextType } from '../context/Session';
import { useNavigate } from 'react-router-dom';

function Login() {
  const { setUser } = useContext(SessionContext) as SessionContextType;
  const navigate = useNavigate();
  const [name, setName] = useState('');
  const [surname, setSurname] = useState('');
  const [github, setGithub] = useState('');
  const [error, setError] = useState('');

  const handleSubmit = (e: React.FormEvent<HTMLFormElement>) => {
    e.preventDefault();
    // Lógica de validación (ejemplo)
    if (name === '' || surname === '' || github === '') {
      setError('Todos los campos son obligatorios');
    } else {
        setError('');
        // Si no hay error, setea el usuario con la información proporcionada
        setUser({
          name,
          surname,
          github,
        });
        navigate("/home");
    }
  };

  return (
    <div className="flex justify-center items-center min-h-screen bg-secondary">
      <div className="bg-primary p-8 rounded-lg shadow-lg w-96">
        <h2 className="text-2xl font-bold text-center mb-6">Login</h2>
        <form onSubmit={handleSubmit}>
          <div className="mb-4">
            <label htmlFor="name" className="block text-sm font-semibold text-gray-700">Name</label>
            <input 
              type="name" 
              id="name" 
              value={name} 
              onChange={(e) => setName(e.target.value)} 
              className="w-full p-3 mt-2 border border-gray-300 rounded-lg focus:outline-none focus:ring-2 focus:ring-blue-500 bg-input"
            />
          </div>
          <div className="mb-4">
            <label htmlFor="surname" className="block text-sm font-semibold text-gray-700">Surname</label>
            <input 
              type="surname" 
              id="surname" 
              value={surname} 
              onChange={(e) => setSurname(e.target.value)} 
              className="w-full p-3 mt-2 border border-gray-300 rounded-lg focus:outline-none focus:ring-2 focus:ring-blue-500 bg-input"
            />
          </div>
          <div className="mb-4">
            <label htmlFor="username" className="block text-sm font-semibold text-gray-700">Username</label>
            <input 
              type="username" 
              id="username" 
              value={github} 
              onChange={(e) => setGithub(e.target.value)} 
              className="w-full p-3 mt-2 border border-gray-300 rounded-lg focus:outline-none focus:ring-2 focus:ring-blue-500 bg-input"
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
      </div>
    </div>
  );
}

export default Login;
