import { defineConfig } from "vite";

// The app is served from this directory; Fable compiles F# into ./build,
// which index.html references as a module entry point.
export default defineConfig({
  root: ".",
  server: {
    port: 5173,
  },
  build: {
    outDir: "dist",
    emptyOutDir: true,
  },
});
