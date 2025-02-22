import { StrictMode } from "react";
import { createRoot } from "react-dom/client";
import "./index.css";
import HomePage from "./pages/HomePage.tsx";
import { SessionProvider } from "./context/Session.tsx";
import { createBrowserRouter, RouterProvider } from "react-router";
import SkillPage from "./pages/SkillPage.tsx";

const router = createBrowserRouter([
  {
    path: "/",
    element: <HomePage />,
    errorElement: <HomePage />,
  },
  {
    path: "/skills/:skill",
    element: <SkillPage />,
  },
]);

createRoot(document.getElementById("root")!).render(
  <StrictMode>
    <SessionProvider>
      {/* <App /> */}
      <RouterProvider router={router} />
    </SessionProvider>
  </StrictMode>
);
