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
requireContract(/<h1[^>]+aria-label="One trap night\. A whole population story\."/.test(html), "poster heading needs one continuous accessible name");
requireContract(/One trap night\. A whole/.test(html) && /population story\./.test(html), "persuasive cover promise missing");
requireContract(/Follow tagged small mammals across years of return visits\./.test(html), "plain-language poster promise missing");
requireContract(/class="poster-cta"[\s\S]*?<span>Pick a place<\/span>/.test(html), "single poster CTA missing");
requireContract(/One animal in a much bigger field story\./.test(html), "brief below-fold suite bridge missing");
requireContract(/href="https:\/\/tgilbert14\.github\.io\/NEON-Driver-Cascade\/"/.test(html), "Driver suite destination missing");
requireContract(!/(Who came back\?|Is the population changing\?|Who shares the night\?|What this can tell you|What this cannot tell you)/.test(html), "retired long-form documentary sections remain");
requireContract(!/(178,216|93,169|2013–2024|hero-facts|signal-bar|measure-grid)/.test(html), "poster must not carry a synthetic metric or methods band");
requireContract(!/\bfetch\s*\(/.test(html), "cover must not make an unsolicited prewarm request");
requireContract(!/(?:href|src)="http:\/\//.test(html), "cover contains an insecure link or asset URL");
requireContract(/rel="canonical" href="https:\/\/tgilbert14\.github\.io\/NEON-Small-Mammal-Tracker-App\/"/.test(html), "canonical URL missing");
requireContract(/og:image:width" content="1200"/.test(html) && /og:image:height" content="630"/.test(html), "social dimensions missing");
requireContract(/og:image:alt/.test(html) && /twitter:image:alt/.test(html), "social alt text missing");
requireContract(/assets\/pacific-pocket-mouse-sherman-trap-usgs\.jpg/.test(html), "documentary hero source missing");
requireContract(/Cheryl Brehme, USGS/.test(html) && /Public domain/.test(html), "visible documentary photo credit missing");
requireContract(!/(hero-mobile-v1|og-habitat-v2|og-card-v3-source)/.test(html), "retired generated artwork is still referenced");
requireContract(!/min-width:\s*320px/.test(html), "fixed 320px body width breaks scrollbar-adjusted reflow");
requireContract(/\.poster-cta \{[\s\S]{0,100}min-height: 52px;/.test(html), "poster CTA must exceed the 44px touch target");
requireContract(/@media \(max-width: 760px\)/.test(html) && /@media \(max-width: 360px\)/.test(html), "mobile and narrow reflow contracts missing");
requireContract(/@media \(prefers-reduced-motion: reduce\)/.test(html), "reduced-motion alternative missing");

const liveLinks = [...html.matchAll(/<a\b[^>]*href="https:\/\/019ec337-7100-317e-5052-c3bf32ffcb79\.share\.connect\.posit\.cloud\/"[^>]*>/g)].map((match) => match[0]);
requireContract(liveLinks.length === 1, `expected one primary live-app control, found ${liveLinks.length}`);
for (const link of liveLinks) {
  requireContract(/target="_blank"/.test(link) && /rel="[^"]*noopener[^"]*"/.test(link), "live-app controls must open safely in a new tab");
}

const ids = [...html.matchAll(/\bid="([^"]+)"/g)].map((match) => match[1]);
requireContract(new Set(ids).size === ids.length, "duplicate HTML id");

requireContract(/class="product-name" aria-current="page">Small Mammal Tracker/.test(html), "Small Mammals current-page marker missing");

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

console.log("Cover contract passed: concise Living Poster, documentary media, one CTA, subtle suite bridge, metadata, no-prewarm, and image dimensions.");
