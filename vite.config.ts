import { defineConfig } from 'vite'

export default defineConfig({
  root: "./modules/examples",
  server: {
    host: "0.0.0.0",
    proxy: {
      "^/fennec/.+": {
        target: "wss://fennec.fly.dev",
        secure: false,
        changeOrigin: true,
        ws: true,
      }
    },
  }
})
