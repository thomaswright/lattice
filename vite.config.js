import { defineConfig } from "vite";
import react from "@vitejs/plugin-react";

// https://vitejs.dev/config/
export default defineConfig({
  base: "https://thomaswright.github.io/lattice",
  plugins: [
    react({
      include: ["**/*.res.mjs"],
    }),
  ],
});
