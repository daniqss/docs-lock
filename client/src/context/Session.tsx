import { createContext, useState } from "react";
import { Skill, User } from "../api/__generated__";

export type SessionContextType = {
  user: User;
  setUser: React.Dispatch<React.SetStateAction<User>>;
  selectedSkill: Skill;
  setSelectedSkill: React.Dispatch<React.SetStateAction<Skill>>;
};

const SessionContext = createContext<SessionContextType | null>(null);

function SessionProvider({ children }: { children: React.ReactNode }) {
  const [user, setUser] = useState<User>({
    id: 0,
    gitUsername: "",
    realName: "",
  });
  const [selectedSkill, setSelectedSkill] = useState<Skill>({
    id: 0,
    skillName: "",
    participants: {
      participants: [],
    },
  });

  return (
    <SessionContext.Provider
      value={{ user, setUser, selectedSkill, setSelectedSkill }}
    >
      {children}
    </SessionContext.Provider>
  );
}

export { SessionContext, SessionProvider };
