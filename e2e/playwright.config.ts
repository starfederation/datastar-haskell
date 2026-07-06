import { defineConfig } from "@playwright/test";

export default defineConfig({
  testDir: "./tests",
  use: {
    baseURL: "http://localhost:3113",
  },
  webServer: {
    // cabal.project.core: e2e-server needs no system libraries, and CI runs
    // this job without any installed.
    command: "cabal run --project-file=cabal.project.core e2e-server",
    cwd: "..",
    url: "http://localhost:3113",
    reuseExistingServer: !process.env.CI,
  },
});
