import { createContext, useState } from "react";
import { User } from "../api/__generated__";

export type SessionContextType = {
  user: User;
  setUser: React.Dispatch<React.SetStateAction<User>>;
};

const SessionContext = createContext<SessionContextType | null>(null);

function SessionProvider({ children }: { children: React.ReactNode }) {
  const [user, setUser] = useState<User>({
    id: 0,
    gitUsername: "",
    realName: "",
  });

  return (
    <SessionContext.Provider value={{ user, setUser }}>
      {children}
    </SessionContext.Provider>
  );
}

export { SessionContext, SessionProvider };
