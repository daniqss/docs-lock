import { useState } from "react";


export default function TextBoxWithArrow() {
  const [text, setText] = useState("");

  const handleSend = async () => {
    if (!text.trim()) return;
    console.log("Enviando a la base de datos:", text);
    setText("");
  };

  return (
    <div style={{ display: "flex", alignItems: "center", gap: "10px", padding: "10px", border: "1px solid #ccc", borderRadius: "8px", maxWidth: "400px" }}>
      <input
        type="text"
        value={text}
        onChange={(e) => setText(e.target.value)}
        placeholder="Escribe algo..."
        style={{ flex: 1, padding: "8px", border: "1px solid #ccc", borderRadius: "4px" }}
      />
      <button onClick={handleSend} style={{ padding: "8px", background: "#007bff", color: "white", border: "none", borderRadius: "4px", cursor: "pointer" }}>
        <PaperIcon className="w-8 h-8 cursor-pointer" />
      </button>
    </div>
  );
}
