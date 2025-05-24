import { useEffect, useRef, useState } from "react";

import init, { wasm_eval } from "../../weblisp-compiler/pkg/weblisp_compiler";
import wasmUrl from "../../weblisp-compiler/pkg/weblisp_compiler_bg.wasm?url";
import starterCode from "./starter.weblisp?raw";

async function evalCode(text: string): Promise<string> {
  await init(wasmUrl);
  return wasm_eval(text);
}

async function submitCode(
  inputValue: string,
  outputRef: React.RefObject<HTMLDivElement | null>
) {
  if (outputRef.current) {
    const output = await evalCode(inputValue);
    outputRef.current.textContent = output;
  }
}

export default function App() {
  const [inputValue, setInputValue] = useState("");
  const outputRef = useRef<HTMLDivElement | null>(null);
  const debounceTimeoutRef = useRef<number | null>(null);

  useEffect(() => {
    const savedValue = localStorage.getItem("inputValue");
    setInputValue(savedValue ?? starterCode);
  }, []);

  useEffect(() => {
    if (debounceTimeoutRef.current) {
      clearTimeout(debounceTimeoutRef.current);
    }

    debounceTimeoutRef.current = setTimeout(() => {
      localStorage.setItem("inputValue", inputValue);
    }, 200);

    return () => {
      if (debounceTimeoutRef.current) {
        clearTimeout(debounceTimeoutRef.current);
      }
    };
  }, [inputValue]);

  return (
    <div className="flex flex-col h-screen w-full">
      <header className="w-full h-20 p-2">
        <button
          className="border w-16 h-8"
          onClick={() => submitCode(inputValue, outputRef)}
        >
          Run
        </button>
      </header>
      <section className="flex w-full h-full font-mono px-2 gap-8">
        <textarea
          className="w-1/2 h-full resize-none"
          spellCheck="false"
          value={inputValue}
          onChange={(e) => setInputValue(e.currentTarget.value)}
        />
        <div className="w-1/2 h-full whitespace-pre-wrap" ref={outputRef} />
      </section>
      <footer className="w-full h-20 p-2">{`Made with <3 using Rust`}</footer>
    </div>
  );
}
