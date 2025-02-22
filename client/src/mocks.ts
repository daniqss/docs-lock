import { Section, UserId } from "./api/__generated__";

export const sections: Section[] = [
  {
    id: 1,
    seccionName: "Hooks",
    skill: {
      name: "React Hooks",
      description:
        "Esencial para gestionar el estado y los efectos secundarios.",
      level: "Intermedio",
    },
    participants: {
      participants: ["1", "2"] as UserId[], // IDs de usuario como strings
    },
  },
  {
    id: 2,
    seccionName: "Context",
    skill: {
      name: "Context API",
      description:
        "Proporciona una forma de compartir datos profundamente en el árbol de componentes.",
      level: "Principiante",
    },
    participants: {
      participants: ["3"] as UserId[], // ID de usuario como string
    },
  },
  {
    id: 3,
    seccionName: "ES6+",
    skill: {
      name: "JavaScript moderno",
      description: "Aprovechando las últimas características de JavaScript.",
      level: "Principiante",
    },
    participants: {
      participants: [] as UserId[], // Sin participantes todavía
    },
  },
  {
    id: 4,
    seccionName: "Async",
    skill: {
      name: "Programación asíncrona",
      description:
        "Manejo de operaciones asíncronas con Promesas y async/await.",
      level: "Intermedio",
    },
    participants: {
      participants: ["1", "2", "3"] as UserId[], // IDs de usuario como strings
    },
  },
  {
    id: 5,
    seccionName: "Performance",
    skill: {
      name: "Optimización del rendimiento",
      description:
        "Técnicas para mejorar el rendimiento de las aplicaciones React.",
      level: "Avanzado",
    },
    participants: {
      participants: ["2"] as UserId[], // ID de usuario como string
    },
  },
  {
    id: 6,
    seccionName: "Testing",
    skill: {
      name: "Prueba de componentes React",
      description:
        "Escritura de pruebas unitarias y de integración para aplicaciones React.",
      level: "Intermedio",
    },
    participants: {
      participants: ["1"] as UserId[], // ID de usuario como string
    },
  },
];
