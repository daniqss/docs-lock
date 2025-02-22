import { skillSection } from "./types";

export const sections: skillSection[] = [
  { name: "Hooks", notes: ["useState para estado", "useEffect para efectos"] },
  {
    name: "Context",
    notes: ["useContext para estado global", "Context.Provider"],
  },
  {
    name: "ES6+",
    notes: ["Arrow functions", "Destructuring", "Spread/rest operator"],
  },
  { name: "Async", notes: ["Promises", "Async/Await"] },
  { name: "Performance", notes: ["React.memo", "useMemo", "useCallback"] },
  { name: "Testing", notes: ["Jest", "React Testing Library", "Cypress"] },
];

export const skills: string[] = [
  "React",
  "JavaScript",
  "TypeScript",
  "Node.js",
  "CSS",
  "HTML",
  "Tailwind",
  "Redux",
  "Next.js",
  "GraphQL",
];
