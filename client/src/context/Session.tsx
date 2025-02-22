import { createContext, useState } from "react";
import { user } from "../types";

export type SessionContextType = {
  user: user;
  setUser: React.Dispatch<React.SetStateAction<user>>;
};

const SessionContext = createContext<SessionContextType | null>(null);

function SessionProvider({ children }: { children: React.ReactNode }) {
  const [user, setUser] = useState<user>({
    name: "",
    surname: "",
    github: "",
  });

  return (
    <SessionContext.Provider value={{ user, setUser }}>
      {children}
    </SessionContext.Provider>
  );
}

export { SessionContext, SessionProvider };
