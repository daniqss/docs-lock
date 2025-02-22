import { StrictMode } from "react";
import { createRoot } from "react-dom/client";
import "./index.css";
import App from "./App.tsx";
import { SessionProvider } from "./context/Session.tsx";
import { createBrowserRouter, RouterProvider } from "react-router";
import SkillPage from "./pages/SkillsPage.tsx";

const router = createBrowserRouter([
  {
    path: "/",
    element: <App />,
    errorElement: <App />,
  },
  {
    path: "/skills/",
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
