import { defineConfig } from "@playwright/test";

export default defineConfig({
  testDir: "./tests",
  use: {
    baseURL: "http://localhost:3113",
  },
  webServer: {
    command: "cabal run e2e-server",
    cwd: "..",
    url: "http://localhost:3113",
    reuseExistingServer: !process.env.CI,
  },
});
