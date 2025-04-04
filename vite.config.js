import { defineConfig } from "vite";
import fable from "vite-plugin-fable";

// https://vitejs.dev/config/
export default defineConfig({
    plugins: [fable()],
    server: {
        port: 8080
    },
    base: "/demo-elmish-polling/"
});