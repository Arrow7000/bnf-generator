import { defineConfig } from "vite";

// The app is served from this directory; Fable compiles F# into ./build,
// which index.html references as a module entry point.
//
// `base` must match the GitHub Pages sub-path for a project site
// (https://<user>.github.io/bnf-generator/). It is overridable via the
// BASE_PATH env var so local `vite preview` / forks can use a different path.
export default defineConfig({
  root: ".",
  base: process.env.BASE_PATH ?? "/bnf-generator/",
  server: {
    port: 5173,
  },
  build: {
    outDir: "dist",
    emptyOutDir: true,
  },
});
