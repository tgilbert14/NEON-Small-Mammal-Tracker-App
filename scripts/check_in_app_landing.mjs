import { existsSync, readFileSync } from "node:fs";
import { createHash } from "node:crypto";

const ui = readFileSync("ui.R", "utf8");
const css = readFileSync("www/styles.css", "utf8");
const provenance = readFileSync("docs/IMAGE-PROVENANCE.md", "utf8");
const asset = "www/assets/small-mammal-field-usgs.jpg";

function requireContract(condition, message) {
  if (!condition) throw new Error(`in-app landing contract: ${message}`);
}

function sha256(path) {
  return createHash("sha256").update(readFileSync(path)).digest("hex");
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
  throw new Error(`in-app landing contract: could not read ${path} dimensions`);
}

requireContract(ui.includes("small_mammal_poster <- function"), "Living Poster helper is missing");
requireContract(ui.includes("One trap night. A whole ") && ui.includes("population story."), "poster hook drifted");
requireContract(ui.includes("Follow tagged small mammals across years of return visits."), "plain-language promise drifted");
requireContract(ui.includes("Pick a place"), "primary poster CTA is missing");
requireContract(ui.includes('href = "#site-picker-start"') && ui.includes('id = "site-picker-start"'), "CTA no longer lands on the site picker");
requireContract(ui.includes('`aria-labelledby` = "smt-poster-title"') && ui.includes('h1(id = "smt-poster-title"'), "poster heading is not named semantically");
requireContract(ui.includes('class = "btn-outline-dark btn-sm tb-help", `aria-label` = "How it works"') && ui.includes('class = "tb-help-label"'), "mobile help control lost its accessible name");
requireContract(ui.includes("Documentary field photograph · not a NEON observation") && ui.includes("Cheryl Brehme · USGS · public domain"), "documentary boundary or credit is missing");
requireContract(ui.includes('asset_url("assets/small-mammal-field-usgs.jpg")'), "runtime documentary asset is not referenced through cache busting");
requireContract(!/(hero-mobile-v1|og-habitat-v2|og-card-v3-source)/.test(ui), "retired generated artwork is referenced by the app landing");
requireContract(!ui.includes('class = "splash-guide"'), "old floating splash mascot still overlays the documentary poster");

for (const id of ["pickerMap", "pickMode", "rangeSpecies", "stateSel", "site", "dateRange", "provisional", "loadBtn", "compareBtn"]) {
  requireContract(ui.includes(`\"${id}\"`), `functional picker control ${id} is missing`);
}

requireContract(/\.smt-poster-cta\s*\{[\s\S]{0,180}min-height:\s*48px/.test(css), "poster CTA is not at least 48px tall");
requireContract(/\.smt-poster-cta\s*\{[\s\S]{0,240}gap:\s*\.35em[\s\S]{0,80}box-sizing:\s*border-box/.test(css), "poster CTA spacing or width containment drifted");
requireContract(/\.smt-poster-photo figcaption\s*\{[\s\S]{0,320}color:\s*#fffef9[\s\S]{0,80}background:\s*rgba\(17, 19, 15, \.9\)/.test(css), "photo credit lost its opaque text or dark contrast scrim");
requireContract(css.includes("@media (max-width: 760px)") && css.includes("@media (max-width: 420px)"), "responsive poster breakpoints are missing");
requireContract(/@media \(prefers-reduced-motion: reduce\)[\s\S]*?\.smt-poster-cta\s*\{\s*transition:\s*none/.test(css), "poster CTA lacks reduced-motion handling");
requireContract(/@media \(max-width: 420px\)[\s\S]{0,120}body\.bslib-page-fill\s*\{[^}]*padding-right:\s*12px;[^}]*padding-left:\s*12px/.test(css), "narrow mobile page gutters drifted");
requireContract(/@media \(max-width: 390px\)[\s\S]{0,120}\.top-bar\s*\{[^}]*flex-wrap:\s*nowrap/.test(css) && /\.top-bar \.tb-theme-lab\s*\{\s*display:\s*none/.test(css), "320px top bar can wrap or retain the redundant theme label");
requireContract(/@media \(max-width: 340px\)[\s\S]{0,180}\.top-bar-brand \.tb-title\s*\{\s*font-size:\s*11\.5px/.test(css), "very-narrow wordmark scale drifted");

requireContract(existsSync(asset), `missing runtime image ${asset}`);
requireContract(sha256(asset) === "e9d0158ac56f95437f0958c5aa4037701c95f2b616e87bb54440ab08e3a50f55", "runtime documentary derivative hash drifted");
const [width, height] = jpegSize(asset);
requireContract(width === 1800 && height === 1350, `runtime documentary derivative is ${width}x${height}, expected 1800x1350`);
requireContract(provenance.includes(asset) && provenance.includes("no generative edit"), "runtime derivative provenance is incomplete");

console.log("In-app landing contract passed: brief Living Poster, documentary provenance, picker continuity, accessibility, and responsive asset.");
