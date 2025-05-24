cd weblisp-compiler
wasm-pack build --target web --out-dir pkg

cd ../weblisp-frontend
npm run build
npx gh-pages -d dist 
