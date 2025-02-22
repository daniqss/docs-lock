import { createContext, useState } from "react";
import { UserType } from "../types/user";

export type SessionContextType = {
  user: UserType;
  setUser: React.Dispatch<React.SetStateAction<UserType>>;
};

const SessionContext = createContext<SessionContextType | null>(null);

function SessionProvider({ children }: { children: React.ReactNode }) {
  const [user, setUser] = useState<UserType>({
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
