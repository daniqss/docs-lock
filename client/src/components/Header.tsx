import { ReactNode } from "react";
import SearchBar from "./SearchBar";
import ProfileIcon from "./icons/ProfileIcon";

type HeaderProps = {
  children: ReactNode;
};

function Header({ children }: HeaderProps) {
  return (
    <header className="flex flex-row justify-between items-center border-b lg:mb-16 lg:px-2 mb-8">
      <section className="flex flex-row space-x-2 items-center lg:my-3 my-1">
        <h1 className="lg:text-4xl text-2xl font-bold text-neutral-300">
          {children}
        </h1>
      </section>
      <SearchBar />
      <section className="flex flex-row space-x-4">
        <ProfileIcon className="w-8 h-8" />
      </section>
    </header>
  );
}

export default Header;
