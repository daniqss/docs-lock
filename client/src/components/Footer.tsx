import GithubIcon from "./icons/GithubIcon";

export default function Footer() {
  return (
    <footer className="footer">
      <div className="footer-content">
        <p>Made in A Coruña (Spain) under MIT license</p>

        <div className="team">
          <span>Paula Carril Gontan, </span>
          <span>Santiago Garea Cidre, </span>
          <span>Javier Manotas Ruiz, </span>
          <span>Daniel Queijo Seoane</span>
        </div>

        <a
          href="https://github.com/daniqss/docs-lock"
          target="_blank"
          rel="noopener noreferrer"
        >
          Ver el repositorinhasdasdasdo en GitHub{" "}
          <GithubIcon className="github-footer-icon" />
        </a>
      </div>
    </footer>
  );
}
