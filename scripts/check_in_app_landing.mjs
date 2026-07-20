import { existsSync, readFileSync, statSync } from "node:fs";
import { createHash } from "node:crypto";

const ui = readFileSync("ui.R", "utf8");
const css = readFileSync("www/styles.css", "utf8");
const provenance = readFileSync("docs/IMAGE-PROVENANCE.md", "utf8");

function requireContract(condition, message) {
  if (!condition) throw new Error(`in-app landing contract: ${message}`);
}

function sha256(path) {
  return createHash("sha256").update(readFileSync(path)).digest("hex");
}

function pngSize(path) {
  const data = readFileSync(path);
  requireContract(data.subarray(1, 4).toString("ascii") === "PNG", `${path} is not PNG`);
  return [data.readUInt32BE(16), data.readUInt32BE(20)];
}

function webpSize(path) {
  const data = readFileSync(path);
  requireContract(data.subarray(0, 4).toString("ascii") === "RIFF" && data.subarray(8, 12).toString("ascii") === "WEBP", `${path} is not WebP`);
  const chunk = data.subarray(12, 16).toString("ascii");
  if (chunk === "VP8 ") return [data.readUInt16LE(26) & 0x3fff, data.readUInt16LE(28) & 0x3fff];
  if (chunk === "VP8X") return [data.readUIntLE(24, 3) + 1, data.readUIntLE(27, 3) + 1];
  throw new Error(`in-app landing contract: unsupported WebP chunk ${chunk} in ${path}`);
}

requireContract(ui.includes("small_mammal_poster <- function"), "Living Poster helper is missing");
requireContract(ui.includes('`aria-label` = "Who moves after dark?"') && ui.includes('tags$span("Who moves"), tags$em("after dark?")'), "approved hook or accessible name drifted");
requireContract(ui.includes("Meet the tiny lives reshaping the landscape."), "approved one-line promise drifted");
requireContract(ui.includes("Meet the mammals"), "approved CTA drifted");
requireContract(ui.includes('href = "#site-picker-start"') && ui.includes('id = "site-picker-start"'), "CTA no longer lands on the site picker");
requireContract(ui.includes('`aria-labelledby` = "smt-poster-title"') && ui.includes('h1(id = "smt-poster-title"'), "poster heading is not named semantically");
requireContract(ui.includes("Desert Data Labs") && ui.includes("Whole suite: ") && ui.includes("https://tgilbert14.github.io/NEON-Driver-Cascade/"), "shared DDL/Driver topline is incomplete");
requireContract(ui.includes("NEON Small Mammal Tracker · unofficial"), "app eyebrow or unofficial boundary missing");
requireContract(ui.includes("Public NEON DP1.10072.001 · sampled capture records—not total population or measured landscape effects"), "first-frame scientific boundary missing");
requireContract(ui.includes("Editorial illustration—not a field photograph or data record."), "illustration disclosure missing");
requireContract(ui.includes("Editorial screenprint of a small nocturnal mouse") && ui.includes("oversized metal box live trap beneath a starry sky."), "art alt text drifted");
requireContract(ui.includes('asset_url("assets/small-mammal-living-poster.png")'), "PNG fallback is not cache-busted");
requireContract(ui.includes('asset_url("assets/small-mammal-living-poster.webp")') && ui.includes('asset_url("assets/small-mammal-living-poster-840.webp")'), "responsive WebP set is incomplete");
requireContract(ui.includes('type = "image/webp"') && ui.includes('sizes = "(max-width: 760px) 100vw, 58vw"'), "responsive picture hints drifted");
requireContract(ui.includes('width = "1672", height = "941", fetchpriority = "high"') && ui.includes('decoding = "async"'), "poster art needs intrinsic dimensions and high-priority async decoding");
requireContract(!/(One trap night|A whole population story|Follow tagged small mammals|small-mammal-field-usgs|Documentary field photograph|Cheryl Brehme)/.test(ui), "retired documentary landing remains live");
requireContract(!ui.includes('class = "splash-guide"'), "old floating splash mascot still overlays the Living Poster");
requireContract(ui.includes('tags$nav(\n          class = "smt-poster-nav", `aria-label` = "NEON Explorer Suite"'), "Driver route is not wrapped in named suite navigation");
requireContract(ui.includes('tabindex = "-1"') && ui.includes("target.focus({preventScroll:true})"), "CTA does not transfer focus to the picker introduction");

requireContract(ui.includes('class = "btn-outline-dark btn-sm tb-help", `aria-label` = "How it works"') && ui.includes('class = "tb-help-label"'), "mobile help control lost its accessible name");
for (const id of ["pickerMap", "pickMode", "rangeSpecies", "stateSel", "site", "dateRange", "provisional", "loadBtn", "compareBtn"]) {
  requireContract(ui.includes(`\"${id}\"`), `functional picker control ${id} is missing`);
}

requireContract(/\.smt-poster\s*\{[\s\S]{0,180}grid-template-columns:\s*minmax\(0, \.42fr\) minmax\(0, \.58fr\)/.test(css), "shared 42/58 poster frame drifted");
requireContract(/\.smt-poster-topline\s*\{[\s\S]{0,220}min-height:\s*44px/.test(css), "compact topline is missing or too small");
requireContract(/\.smt-poster-suite-link\s*\{[\s\S]{0,160}min-height:\s*44px/.test(css), "Driver suite jump lacks a 44px target");
requireContract(/\.picker-start:focus-visible\s*\{[^}]*outline:\s*3px solid var\(--sky\)/.test(css), "picker focus destination lacks a visible indicator");
requireContract(/\.smt-poster h1\s*\{[\s\S]{0,260}font-family:\s*Iowan Old Style/.test(css), "editorial serif headline drifted");
requireContract(/\.smt-poster-cta\s*\{[\s\S]{0,180}min-height:\s*48px/.test(css), "poster CTA is not at least 48px tall");
requireContract(/\.smt-poster-cta\s*\{[\s\S]{0,260}gap:\s*\.35em[\s\S]{0,100}box-sizing:\s*border-box/.test(css), "poster CTA spacing or width containment drifted");
requireContract(/\.smt-poster-art figcaption\s*\{[\s\S]{0,420}color:\s*#f3e8cb[\s\S]{0,100}background:\s*rgba\(17, 21, 18, \.9\)/.test(css), "art disclosure lost its contrast scrim");
requireContract(css.includes("@media (max-width: 760px)") && css.includes("@media (max-width: 420px)") && css.includes("@media (max-width: 350px)"), "responsive poster seams are incomplete");
requireContract(/@media \(max-width: 760px\)[\s\S]{0,360}\.smt-poster-art\s*\{[^}]*grid-row:\s*1/.test(css), "mobile art-first order drifted");
requireContract(/@media \(prefers-reduced-motion: reduce\)[\s\S]*?\.smt-poster-cta\s*\{\s*transition:\s*none/.test(css), "poster CTA lacks reduced-motion handling");
requireContract(/@media \(max-width: 420px\)[\s\S]{0,120}body\.bslib-page-fill\s*\{[^}]*padding-right:\s*12px;[^}]*padding-left:\s*12px/.test(css), "narrow mobile page gutters drifted");
requireContract(/@media \(max-width: 390px\)[\s\S]{0,120}\.top-bar\s*\{[^}]*flex-wrap:\s*nowrap/.test(css) && /\.top-bar \.tb-theme-lab\s*\{\s*display:\s*none/.test(css), "320px top bar can wrap or retain the redundant theme label");
requireContract(/@media \(max-width: 340px\)[\s\S]{0,180}\.top-bar-brand \.tb-title\s*\{\s*font-size:\s*11\.5px/.test(css), "very-narrow wordmark scale drifted");

const assets = {
  "www/assets/small-mammal-living-poster.png": "b414540c6ec0172ee586ef016fc204cb2aef7bc60ad73f7dc6f8f0ea27372ef1",
  "www/assets/small-mammal-living-poster.webp": "7a1f9c78895868d0e540ad3126cf5af6b46a25ed4d02b8cf9113fa613cf2fe29",
  "www/assets/small-mammal-living-poster-840.webp": "5fc560601bbb0146ffdfc03e2cf662bc7a8e20dce790f52f218ea1bdba8904a5"
};
for (const [path, expected] of Object.entries(assets)) {
  requireContract(existsSync(path), `missing runtime image ${path}`);
  requireContract(sha256(path) === expected, `${path} hash drifted`);
  requireContract(provenance.includes(path) && provenance.includes(expected), `${path} provenance is incomplete`);
}

requireContract(pngSize("www/assets/small-mammal-living-poster.png").join("x") === "1672x941", "runtime PNG dimensions drifted");
requireContract(webpSize("www/assets/small-mammal-living-poster.webp").join("x") === "1672x941", "runtime full WebP dimensions drifted");
requireContract(webpSize("www/assets/small-mammal-living-poster-840.webp").join("x") === "840x473", "runtime compact WebP dimensions drifted");
requireContract(statSync("www/assets/small-mammal-living-poster.webp").size < 600_000, "runtime full WebP exceeds 600 KB");
requireContract(statSync("www/assets/small-mammal-living-poster-840.webp").size < 140_000, "runtime compact WebP exceeds 140 KB");

console.log("In-app landing contract passed: approved screenprint, Suite Living Poster frame, one invitation, picker continuity, boundary copy, responsive media, and accessibility.");
