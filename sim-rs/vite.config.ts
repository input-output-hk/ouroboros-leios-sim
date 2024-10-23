import { defineConfig } from 'vite';
import wasm from 'vite-plugin-wasm';
import topLevelAwait from 'vite-plugin-top-level-await';

export default defineConfig({
  build: {
    minify: false
  },
  plugins: [
    wasm(),
    topLevelAwait(),
  ]
});
