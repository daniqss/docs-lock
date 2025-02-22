import { useState, useRef, useEffect, useContext } from "react";
import ProfileIcon from "./icons/ProfileIcon";
import { SessionContext, SessionContextType } from "../context/Session";
import { Link } from "react-router-dom";

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
    <header className="flex flex-row justify-between items-center border-b lg:mt-4 lg:px-2 relative">
      <section className="flex flex-row space-x-2 items-center lg:my-3 my-1">
        <Link to="/">
          <h1 className="lg:text-4xl text-2xl font-bold text-neutral-300">
            docs.lock
          </h1>
        </Link>
      </section>
      <section className="flex flex-row space-x-4 relative">
        {/* Icono del perfil con evento de clic */}
        <button
          onClick={() => setDropdownOpen(!isDropdownOpen)}
          className="relative"
        >
          <ProfileIcon className="w-8 h-8 cursor-pointer" />
        </button>

        {/* Dropdown */}
        {isDropdownOpen && (
         <div
         ref={dropdownRef}
         className="absolute right-0 top-12 bg-white shadow-lg rounded-md p-3 w-56 border text-left"
       >
         <p className="text-gray-900 font-semibold">
           {"name: "} {user.name} {user.surname}
         </p>
         <p className="text-gray-600 text-sm">
           {"github: "} {user.github}
         </p>
       </div>
       
        )}
      </section>
    </header>
  );
}

export default Header;
