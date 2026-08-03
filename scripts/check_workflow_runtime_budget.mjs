import fs from "node:fs";

const PINNED_SETUP_R_DEPS =
  "r-lib/actions/setup-r-dependencies@d3c5be51b12e724e68f33216ca3c148b66d5f0b6";
const SHARED_CACHE_VERSION = "small-mammal-geo-closure-v4";

function fail(message) {
  throw new Error(`Workflow runtime-budget contract failed: ${message}`);
}

function read(path) {
  return fs.readFileSync(path, "utf8").replace(/\r\n/g, "\n");
}

function jobBlock(path, jobName) {
  const source = read(path);
  const marker = `  ${jobName}:\n`;
  const start = source.indexOf(marker);
  if (start < 0) fail(`${path} lacks jobs.${jobName}`);

  const rest = source.slice(start + marker.length);
  const nextJob = rest.search(/^  [A-Za-z0-9_-]+:\n/m);
  return nextJob < 0 ? rest : rest.slice(0, nextJob);
}

function timeoutMinutes(path, jobName) {
  const block = jobBlock(path, jobName);
  const match = block.match(/^    timeout-minutes:\s*(\d+)\s*$/m);
  if (!match) fail(`${path} jobs.${jobName} lacks an explicit timeout`);
  return Number(match[1]);
}

function dependencySteps(path, jobName) {
  const block = jobBlock(path, jobName);
  return block
    .split(/^      - /m)
    .filter((step) => step.includes(`uses: ${PINNED_SETUP_R_DEPS}`));
}

function requireCachedClosure(path, jobName, expectedCacheVersion) {
  const steps = dependencySteps(path, jobName);
  if (steps.length !== 1) {
    fail(`${path} jobs.${jobName} must have exactly one pinned setup-r-dependencies step`);
  }

  const step = steps[0];
  if (!step.includes(`cache-version: ${expectedCacheVersion}`)) {
    fail(`${path} jobs.${jobName} must use cache-version ${expectedCacheVersion}`);
  }
  if (!/^\s{10}cache:\s*always\s*$/m.test(step)) {
    fail(`${path} jobs.${jobName} must persist and restore the exact closure cache`);
  }
}

const ci = ".github/workflows/ci.yml";
const regeneration = ".github/workflows/regenerate-manifest.yml";
const refresh = ".github/workflows/refresh-data.yml";

const ciBudget = timeoutMinutes(ci, "contracts");
const regenerationBudget = timeoutMinutes(regeneration, "generate");
if (ciBudget !== 100 || regenerationBudget !== 100 || ciBudget !== regenerationBudget) {
  fail(
    `CI and manual regeneration must share the 100-minute cold-build budget; got ${ciBudget}/${regenerationBudget}`,
  );
}
if (timeoutMinutes(refresh, "build_candidate") < 100) {
  fail(`${refresh} jobs.build_candidate must retain at least a 100-minute cold-build budget`);
}

requireCachedClosure(ci, "contracts", SHARED_CACHE_VERSION);
requireCachedClosure(regeneration, "generate", SHARED_CACHE_VERSION);
requireCachedClosure(refresh, "build_candidate", "small-mammal-refresh-geo-closure-v4");

console.log(
  "Workflow runtime-budget contract passed: 100-minute PR/regeneration budgets and always-on exact-closure caches.",
);
