import { StrictMode } from "react";
import { createRoot } from "react-dom/client";
import "./index.css";
import HomePage from "./pages/HomePage.tsx";
import { SessionProvider } from "./context/Session.tsx";
import { createBrowserRouter, RouterProvider } from "react-router";
import SkillPage from "./pages/SkillPage.tsx";
import LoginPage from "./pages/LoginPage.tsx";
import { QueryClientProvider } from "@tanstack/react-query";
import queryClient from "./api/QueryClient.ts";

const router = createBrowserRouter([
  {
    path: "/",
    element: <LoginPage />,
  },
  {
    path: "/home",
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
      <QueryClientProvider client={queryClient}>
        <RouterProvider router={router} />
      </QueryClientProvider>
    </SessionProvider>
  </StrictMode>
);
