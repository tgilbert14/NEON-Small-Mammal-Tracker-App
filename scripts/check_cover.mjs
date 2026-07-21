import { existsSync, readFileSync, statSync } from "node:fs";
import { createHash } from "node:crypto";

const html = readFileSync("docs/index.html", "utf8");
const socialSource = readFileSync("docs/social-card.html", "utf8");
const provenance = readFileSync("docs/IMAGE-PROVENANCE.md", "utf8");

function requireContract(condition, message) {
  if (!condition) throw new Error(`cover contract: ${message}`);
}

function count(pattern) {
  return (html.match(pattern) || []).length;
}

function sha256(path) {
  return createHash("sha256").update(readFileSync(path)).digest("hex");
}

function pngSize(path) {
  const data = readFileSync(path);
  requireContract(data.subarray(1, 4).toString("ascii") === "PNG", `${path} is not PNG`);
  return [data.readUInt32BE(16), data.readUInt32BE(20)];
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

function webpSize(path) {
  const data = readFileSync(path);
  requireContract(data.subarray(0, 4).toString("ascii") === "RIFF" && data.subarray(8, 12).toString("ascii") === "WEBP", `${path} is not WebP`);
  const chunk = data.subarray(12, 16).toString("ascii");
  if (chunk === "VP8 ") return [data.readUInt16LE(26) & 0x3fff, data.readUInt16LE(28) & 0x3fff];
  if (chunk === "VP8X") return [data.readUIntLE(24, 3) + 1, data.readUIntLE(27, 3) + 1];
  throw new Error(`cover contract: unsupported WebP chunk ${chunk} in ${path}`);
}

// Suite Living Poster V1 structure and copy budget.
requireContract(count(/<h1\b/gi) === 1, "must contain exactly one h1");
requireContract(count(/<main\b/gi) === 1, "must contain exactly one main landmark");
requireContract(/class="skip"[^>]+href="#main"/.test(html), "missing skip link");
requireContract(/<main id="main" class="poster" tabindex="-1">/.test(html), "poster main is not a focusable skip target");
requireContract(/<header class="topline">/.test(html), "shared Living Poster topline missing");
requireContract(/<nav class="cover-nav" aria-label="NEON Explorer Suite">/.test(html), "suite navigation needs an accessible name");
requireContract(count(/class="suite-jump"/g) === 1, "poster needs exactly one Driver suite jump");
requireContract(/class="suite-jump" href="https:\/\/tgilbert14\.github\.io\/NEON-Driver-Cascade\/"/.test(html), "Driver suite destination missing");
requireContract(/<div class="poster-grid">[\s\S]*?<div class="poster-copy">[\s\S]*?<figure class="poster-art">/.test(html), "shared copy-and-art frame drifted");
requireContract(/<h1 aria-label="Who moves after dark\?">\s*<span>Who moves<\/span><em>after dark\?<\/em><\/h1>/.test(html), "approved hook or accessible name drifted");
requireContract(/<p class="promise">Meet the tiny lives reshaping the landscape\.<\/p>/.test(html), "approved one-line promise drifted");
requireContract(count(/class="button"/g) === 1 && /Meet the mammals/.test(html), "poster needs the one approved CTA");
requireContract(!/(suite-bridge|feature-card|hero-facts|signal-bar|measure-grid)/.test(html), "poster contains a retired marketing or metric band");
requireContract(!/(One trap night|A whole population story|Follow tagged small mammals)/.test(html), "retired documentary copy remains");

// Art authority and scientific boundary.
requireContract(/<picture>[\s\S]*?small-mammal-living-poster-840\.webp 840w,[\s\S]*?small-mammal-living-poster\.webp 1672w[\s\S]*?small-mammal-living-poster\.png/.test(html), "responsive Living Poster art set is incomplete");
requireContract(/width="1672" height="941"[\s\S]{0,120}fetchpriority="high" decoding="async"/.test(html), "poster art needs intrinsic dimensions and high-priority async decoding");
requireContract(/alt="Editorial screenprint of a small nocturnal mouse emerging from an oversized metal box live trap beneath a starry sky\."/.test(html), "art alt text drifted");
requireContract(/Editorial illustration—not a field photograph or data record\./.test(html), "visible illustration boundary missing");
requireContract(!/(pacific-pocket-mouse-sherman-trap-usgs|small-mammal-field-usgs|Photo source|Cheryl Brehme)/.test(html), "historical documentary media remains live");

// Footer honesty and metadata.
requireContract(/Explore 46 places/.test(html) && /DP1\.10072\.001/.test(html) && /CC BY 4\.0/.test(html), "scope or data authority missing");
requireContract(/unofficial and not endorsed by NEON, Battelle, or the NSF/.test(html), "independent-project boundary missing");
requireContract(/<summary>What am I looking at\?<\/summary>/.test(html), "collapsed honesty note missing");
requireContract(/sampled plots and trapping nights/.test(html) && /show observed return visits and sampled community patterns/.test(html), "capture/recapture scope is incomplete");
requireContract(/The tagline is an ecological invitation; these records do not measure landscape effects or ecosystem engineering\./.test(html), "artistic tagline is not separated from measured effects");
requireContract(/"description": "Explore public NEON small-mammal capture and recapture records\."/.test(html), "structured metadata overstates the data");
requireContract(/href="https:\/\/github\.com\/tgilbert14\/NEON-Small-Mammal-Tracker-App">Source<\/a>/.test(html), "source link missing");
requireContract(/href="mailto:desertdatalabs@gmail\.com\?subject=NEON%20Small%20Mammal%20Tracker">Feedback<\/a>/.test(html), "feedback link missing");
requireContract(/rel="icon" href="data:image\/svg\+xml,/.test(html), "local inline favicon missing");
requireContract(/rel="canonical" href="https:\/\/tgilbert14\.github\.io\/NEON-Small-Mammal-Tracker-App\/"/.test(html), "canonical URL missing");
requireContract(/og:title" content="Who moves after dark\?"/.test(html) && /og:description" content="Meet the tiny lives reshaping the landscape\."/.test(html), "social copy drifted");
requireContract(/og:image" content="https:\/\/tgilbert14\.github\.io\/NEON-Small-Mammal-Tracker-App\/og-image\.png"/.test(html), "social image URL drifted");
requireContract(/og:image:width" content="1200"/.test(html) && /og:image:height" content="630"/.test(html), "social dimensions missing");
requireContract(/property="og:image:alt" content="[^"]+"/.test(html) && /name="twitter:image:alt" content="[^"]+"/.test(html), "social alt metadata missing or has empty content");

// Safety, accessibility, and responsive seams.
requireContract(!/\bfetch\s*\(/.test(html), "cover must not make an unsolicited prewarm request");
requireContract(!/(?:href|src)="http:\/\//.test(html), "cover contains an insecure link or asset URL");
requireContract(!/fonts\.(?:googleapis|gstatic)\.com/.test(html), "cover must not depend on an external font request");
requireContract(!/min-width:\s*320px/.test(html), "fixed 320px body width breaks scrollbar-adjusted reflow");
requireContract(/\.button \{[\s\S]{0,120}min-height:\s*52px/.test(html), "CTA must exceed the 44px touch target");
requireContract(/\.suite-jump \{[\s\S]{0,100}min-height:\s*44px/.test(html), "suite jump lacks a 44px touch target");
requireContract(/\.honesty summary \{[\s\S]{0,120}min-height:\s*44px/.test(html), "honesty disclosure lacks a 44px touch target");
requireContract(/\.footer-links a \{[\s\S]{0,100}min-height:\s*44px/.test(html), "footer links lack 44px touch targets");
requireContract(/\.art-note \{[\s\S]{0,460}background:\s*rgba\(17, 21, 18, \.86\)/.test(html), "illustration disclosure lacks a reliable contrast scrim");
requireContract(/@media \(max-width: 700px\)/.test(html) && /@media \(max-width: 420px\) and \(max-height: 860px\)/.test(html) && /@media \(max-width: 340px\)/.test(html), "responsive seams are incomplete");
requireContract(/@media \(prefers-reduced-motion: reduce\)/.test(html), "reduced-motion alternative missing");
requireContract(/@media \(prefers-contrast: more\)/.test(html) && /@media \(forced-colors: active\)/.test(html), "high-contrast accommodations missing");

const liveLinks = [...html.matchAll(/<a\b[^>]*href="https:\/\/019ec337-7100-317e-5052-c3bf32ffcb79\.share\.connect\.posit\.cloud\/"[^>]*>/g)].map((match) => match[0]);
requireContract(liveLinks.length === 1, `expected one live-app CTA, found ${liveLinks.length}`);
requireContract(!/target="_blank"/.test(liveLinks[0]), "primary CTA should preserve same-tab continuity");

const ids = [...html.matchAll(/\bid="([^"]+)"/g)].map((match) => match[1]);
requireContract(new Set(ids).size === ids.length, "duplicate HTML id");

// Reviewed, self-contained media.
const assets = [
  "docs/assets/small-mammal-living-poster-concept.jpg",
  "docs/assets/small-mammal-living-poster.png",
  "docs/assets/small-mammal-living-poster.webp",
  "docs/assets/small-mammal-living-poster-840.webp",
  "docs/social-card.html",
  "docs/og-image.png"
];
for (const path of assets) requireContract(existsSync(path), `missing image asset ${path}`);

const expectedHashes = {
  "docs/assets/small-mammal-living-poster-concept.jpg": "9fd6cd3d5e4fe2d54156aadb373bae94f8acc0cd526f963a94ae53de25cdd42a",
  "docs/assets/small-mammal-living-poster.png": "b414540c6ec0172ee586ef016fc204cb2aef7bc60ad73f7dc6f8f0ea27372ef1",
  "docs/assets/small-mammal-living-poster.webp": "7a1f9c78895868d0e540ad3126cf5af6b46a25ed4d02b8cf9113fa613cf2fe29",
  "docs/assets/small-mammal-living-poster-840.webp": "5fc560601bbb0146ffdfc03e2cf662bc7a8e20dce790f52f218ea1bdba8904a5",
  "docs/social-card.html": "fed45cb67e270e8f8de92d93a5c59d9b8f1734bdd75c14120edf999da6956a02",
  "docs/og-image.png": "8001d35b5e905570773a1fb4916ec8ac78ce101e6de0f8c7eb3f0a552951f8b4"
};
for (const [path, expected] of Object.entries(expectedHashes)) {
  requireContract(sha256(path) === expected, `${path} does not match its reviewed provenance hash`);
  requireContract(provenance.includes(expected), `${path} hash is missing from provenance`);
}

requireContract(jpegSize("docs/assets/small-mammal-living-poster-concept.jpg").join("x") === "900x600", "approved concept dimensions drifted");
requireContract(pngSize("docs/assets/small-mammal-living-poster.png").join("x") === "1672x941", "production art dimensions drifted");
requireContract(webpSize("docs/assets/small-mammal-living-poster.webp").join("x") === "1672x941", "full WebP dimensions drifted");
requireContract(webpSize("docs/assets/small-mammal-living-poster-840.webp").join("x") === "840x473", "compact WebP dimensions drifted");
requireContract(pngSize("docs/og-image.png").join("x") === "1200x630", "social image must be 1200x630");
requireContract(statSync("docs/assets/small-mammal-living-poster.webp").size < 600_000, "full WebP exceeds 600 KB");
requireContract(statSync("docs/assets/small-mammal-living-poster-840.webp").size < 140_000, "compact WebP exceeds 140 KB");
requireContract(statSync("docs/og-image.png").size < 1_300_000, "social image exceeds 1.3 MB");

requireContract(/Who moves[\s\S]*after dark\?/.test(socialSource) && /Meet the tiny lives reshaping the landscape\./.test(socialSource), "social-card source copy drifted");
requireContract(/small-mammal-living-poster\.png/.test(socialSource), "social-card source lost the approved art");

console.log("Cover contract passed: Suite Living Poster V1 frame, approved screenprint, one hook/promise/CTA, honesty boundary, metadata, responsive media, and accessibility.");
