import GithubIcon from "./icons/GithubIcon";

export default function Footer() {
  return (
    <footer className="flex justify-between items-center px-6 py-4 border-t">
  
      <p className="text-sm text-gray-600">Made in A Coruña (Spain) under MIT license</p>
  
      <div className="text-sm text-center text-gray-600">
        <span>Paula Carril Gontan, </span>
        <span>Santiago Garea Cidre, </span>
        <span>Javier Manotas Ruiz, </span>
        <span>Daniel Queijo Seoane</span>
      </div>
  
      <a
        href="https://github.com/daniqss/docs-lock"
        target="_blank"
        rel="noopener noreferrer"
        className="flex items-center space-x-2 text-blue-600 hover:underline"
      >
        <GithubIcon className="w-5 h-5" />
        <span>Ver el repositorio en GitHub</span>
      </a>
    </footer>
  );
  
}
