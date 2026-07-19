import { existsSync, readFileSync } from "node:fs";
import { createHash } from "node:crypto";

const htmlPath = "docs/index.html";
const html = readFileSync(htmlPath, "utf8");

function requireContract(condition, message) {
  if (!condition) throw new Error(`cover contract: ${message}`);
}

function count(pattern) {
  return (html.match(pattern) || []).length;
}

function jpegSize(path) {
  const data = readFileSync(path);
  requireContract(data[0] === 0xff && data[1] === 0xd8, `${path} is not JPEG`);
  let offset = 2;
  while (offset + 9 < data.length) {
    if (data[offset] !== 0xff) { offset += 1; continue; }
    const marker = data[offset + 1];
    if ([0xc0, 0xc1, 0xc2, 0xc3, 0xc5, 0xc6, 0xc7, 0xc9, 0xca, 0xcb, 0xcd, 0xce, 0xcf].includes(marker)) {
      return [data.readUInt16BE(offset + 7), data.readUInt16BE(offset + 5)];
    }
    if (marker === 0xd8 || marker === 0xd9) { offset += 2; continue; }
    const length = data.readUInt16BE(offset + 2);
    requireContract(length >= 2, `${path} has an invalid JPEG segment`);
    offset += 2 + length;
  }
  throw new Error(`cover contract: could not read ${path} dimensions`);
}

function sha256(path) {
  return createHash("sha256").update(readFileSync(path)).digest("hex");
}

requireContract(count(/<h1\b/gi) === 1, "must contain exactly one h1");
requireContract(count(/<main\b/gi) === 1, "must contain exactly one main landmark");
requireContract(/class="skip-link"[^>]+href="#main"/.test(html), "missing skip link");
requireContract(/<nav[^>]+aria-label="Cover navigation"/.test(html), "navigation needs an accessible name");
requireContract(/aria-current="page"/.test(html), "current suite product is not identified");
requireContract(/One trap night\. A whole/.test(html) && /population story\./.test(html), "persuasive cover promise missing");
requireContract(/Who came back\?/.test(html) && /Is the population changing\?/.test(html) && /Who shares the night\?/.test(html), "question-led entry points missing");
requireContract(/What this can tell you/.test(html) && /What this cannot tell you/.test(html), "claim boundary pair missing");
requireContract(/Last production verification: July 18, 2026/.test(html), "dated semantic verification missing");
requireContract(/46/.test(html) && /178,216/.test(html) && /93,169/.test(html) && /145/.test(html) && /2013–2024/.test(html), "verified facts or vintage missing");
requireContract(!/\bfetch\s*\(/.test(html), "cover must not make an unsolicited prewarm request");
requireContract(!/(?:href|src)="http:\/\//.test(html), "cover contains an insecure link or asset URL");
requireContract(/rel="canonical" href="https:\/\/tgilbert14\.github\.io\/NEON-Small-Mammal-Tracker-App\/"/.test(html), "canonical URL missing");
requireContract(/og:image:width" content="1200"/.test(html) && /og:image:height" content="630"/.test(html), "social dimensions missing");
requireContract(/og:image:alt/.test(html) && /twitter:image:alt/.test(html), "social alt text missing");
requireContract(/assets\/pacific-pocket-mouse-sherman-trap-usgs\.jpg/.test(html), "documentary hero source missing");
requireContract(/Cheryl Brehme, USGS/.test(html) && /Public domain/.test(html), "visible documentary photo credit missing");
requireContract(!/(hero-mobile-v1|og-habitat-v2|og-card-v3-source)/.test(html), "retired generated artwork is still referenced");
requireContract(!/min-width:\s*320px/.test(html), "fixed 320px body width breaks scrollbar-adjusted reflow");
requireContract(/\.nav-links \.nav-open \{[\s\S]{0,80}min-height: 44px;/.test(html), "mobile launch target must be at least 44px high");

const liveLinks = [...html.matchAll(/<a\b[^>]*href="https:\/\/019ec337-7100-317e-5052-c3bf32ffcb79\.share\.connect\.posit\.cloud\/"[^>]*>/g)].map((match) => match[0]);
requireContract(liveLinks.length === 6, `expected six purposeful live-app controls, found ${liveLinks.length}`);
for (const link of liveLinks) {
  requireContract(/target="_blank"/.test(link) && /rel="[^"]*noopener[^"]*"/.test(link), "live-app controls must open safely in a new tab");
}

const ids = [...html.matchAll(/\bid="([^"]+)"/g)].map((match) => match[1]);
requireContract(new Set(ids).size === ids.length, "duplicate HTML id");

const suiteUrls = [
  "NEON-Driver-Cascade/",
  "NEON-Plant-Phenology-Explorer/",
  "NEON-Plant-Diversity/",
  "NEON-Vegetation-Structure-Explorer/",
  "NEON-Ground-Beetle-Tracker/",
  "NEON-Mosquito-Pulse/",
  "NEON-Breeding-Birds/",
  "NEON-WaterChemistry-Analyte-Viewer-App/",
  "NEON-My-Little-Inverts/"
];
for (const url of suiteUrls) requireContract(html.includes(url), `missing suite destination ${url}`);
requireContract(/class="suite-link current"[^>]+aria-current="page"[^>]*href="#main"/.test(html), "Small Mammals current-page marker missing");

for (const path of ["docs/assets/pacific-pocket-mouse-sherman-trap-usgs.jpg", "docs/og-card-v4-source.html", "docs/og-image.jpg"]) {
  requireContract(existsSync(path), `missing image asset ${path}`);
}

const expectedHashes = {
  "docs/assets/pacific-pocket-mouse-sherman-trap-usgs.jpg": "b678af26331a98747eba64c382f1ee7e2e1f7c1f77baecbd81dd46622a9e950a",
  "docs/og-image.jpg": "b36c62094270bf1598f38c6162ea6fda1b5c607f513a3faeea4ed62819fdf19a"
};
for (const [path, expected] of Object.entries(expectedHashes)) {
  requireContract(sha256(path) === expected, `${path} does not match its reviewed provenance hash`);
}

const [socialWidth, socialHeight] = jpegSize("docs/og-image.jpg");
requireContract(socialWidth === 1200 && socialHeight === 630, `social image is ${socialWidth}x${socialHeight}, expected 1200x630`);
const [heroWidth, heroHeight] = jpegSize("docs/assets/pacific-pocket-mouse-sherman-trap-usgs.jpg");
requireContract(heroWidth === 4000 && heroHeight === 3000, `documentary hero is ${heroWidth}x${heroHeight}, expected 4000x3000`);

console.log("Cover contract passed: documentary media, persuasive entries, claim boundaries, suite registry, metadata, no-prewarm, and image dimensions.");
