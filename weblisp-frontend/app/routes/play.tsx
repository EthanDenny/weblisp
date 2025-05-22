import { useEffect, useRef, useState } from "react";
import type { Route } from "./+types/home";

import init, {
  wasm_eval,
} from "../../../weblisp-compiler/pkg/weblisp_compiler";
import wasmUrl from "../../../weblisp-compiler/pkg/weblisp_compiler_bg.wasm?url";

const starterCode = `
(def sqrt (x) (
    (def inner_sqrt (x n) (
        (let n_squared (* n n))
        (if (= n_squared x)
            n
            (if (> n_squared x)
                (- n 1)
                (inner_sqrt x (+ n 1))
            )
        )
    ))
    (inner_sqrt x 0)
))

(sqrt 0)
(sqrt 1)
(sqrt 4)
(sqrt 10)
`.slice(1);

export function meta({}: Route.MetaArgs) {
  return [
    { title: "Play" },
    { name: "description", content: "Play with Weblisp!" },
  ];
}

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

export default function Play() {
  const [inputValue, setInputValue] = useState("");
  const outputRef = useRef<HTMLDivElement | null>(null);
  const debounceTimeoutRef = useRef<NodeJS.Timeout | null>(null);

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
