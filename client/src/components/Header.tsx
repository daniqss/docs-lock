import { useState, useRef, useEffect, useContext } from "react";
import ProfileIcon from "./icons/ProfileIcon";
import { SessionContext, SessionContextType } from "../context/Session";
import { Link } from "react-router-dom";
// Add the font import in your CSS file or HTML file

function Header() {
  const [isDropdownOpen, setDropdownOpen] = useState(false);
  const dropdownRef = useRef<HTMLDivElement | null>(null);
  const { user } = useContext(SessionContext) as SessionContextType;

  // Cerrar el dropdown si se hace clic fuera
  useEffect(() => {
    function handleClickOutside(event: MouseEvent) {
      if (
        dropdownRef.current &&
        !dropdownRef.current.contains(event.target as Node)
      ) {
        setDropdownOpen(false);
      }
    }
    document.addEventListener("mousedown", handleClickOutside);
    return () => {
      document.removeEventListener("mousedown", handleClickOutside);
    };
  }, []);
  return (
    <header className="px-22 flex flex-row justify-between items-center w-full h-22 fixed top-0 left-0 border-b shadow-md bg-white ">
      {/* Logo y título */}
      <section className="flex flex-row space-x-2 items-center">
        <Link to="/">
          <h1 className="text-2xl lg:text-4xl font-bold text-gray-900 custom-font">
            docs.lock
          </h1>
        </Link>
      </section>

      {/* Sección del perfil */}
      <section className="flex flex-row space-x-4 relative">
        <button
          onClick={() => setDropdownOpen(!isDropdownOpen)}
          className="relative focus:outline-none active:transform-none"
        >
          <ProfileIcon className="w-8 h-8 cursor-pointer" />
        </button>

        {isDropdownOpen && (
          <div
            ref={dropdownRef}
            className="absolute right-0 top-12 shadow-lg rounded-md p-3 w-56 border bg-white"
          >
            <p className="font-semibold text-gray-900 text-left">
              {"name: "} {user.realName}
            </p>
            <p className="text-sm text-gray-700 text-left">
              {"github: "} {user.gitUsername}
            </p>
          </div>
        )}
      </section>
    </header>
  );
}

export default Header;
