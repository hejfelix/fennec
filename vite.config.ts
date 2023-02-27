import { defineConfig } from 'vite'

export default defineConfig({
  root: "./modules/examples",
  server: {
    host: "0.0.0.0",
    proxy: {
      "^/fennec/.+": {
        target: "ws://127.0.0.1:8080",
        ws: true,
      }
    },
  }
})
