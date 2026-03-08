import { test, expect } from "@playwright/test";

test.beforeEach(async ({ page }) => {
  await page.goto("/");
  // Wait for Datastar to initialize — it processes data-text on the
  // ps-result div, replacing the inner HTML with the signal value.
  await expect(page.locator("#ps-result")).toHaveText("Waiting...", {
    timeout: 10_000,
  });
});

test("PatchElements: server sends HTML fragment that morphs into the DOM", async ({
  page,
}) => {
  await expect(page.locator("#pe-result")).toHaveText("Waiting...");
  await page.click("#pe-trigger");
  await expect(page.locator("#pe-result")).toHaveText("Patched Content");
});

test("PatchSignals: server updates a signal and data-text reflects it", async ({
  page,
}) => {
  await expect(page.locator("#ps-result")).toHaveText("Waiting...");
  await page.click("#ps-trigger");
  await expect(page.locator("#ps-result")).toHaveText("Signal Updated");
});

test("ExecuteScript: server sends JavaScript that mutates the DOM", async ({
  page,
}) => {
  await expect(page.locator("#es-result")).toHaveText("Waiting...");
  await page.click("#es-trigger");
  await expect(page.locator("#es-result")).toHaveText("Script Executed");
});

test("readSignals round-trip: server reads browser signal and echoes it back", async ({
  page,
}) => {
  await expect(page.locator("#rs-result")).toHaveText("Waiting...");
  await page.click("#rs-trigger");
  await expect(page.locator("#rs-result")).toHaveText("Hello from Browser");
});

test("Multiple events: server sends 3 patches, final state wins", async ({
  page,
}) => {
  await expect(page.locator("#me-result")).toHaveText("Waiting...");
  await page.click("#me-trigger");
  await expect(page.locator("#me-result")).toHaveText("Event 3");
});
