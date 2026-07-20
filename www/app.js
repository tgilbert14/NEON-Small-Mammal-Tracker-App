/* =========================================================================
   app.js — count-up stat counters + celebratory confetti
   ========================================================================= */

// ---- animated count-up for the hero stat band ----------------------------
function animateCount(el) {
  if (el.dataset.animated === "1") return;
  el.dataset.animated = "1";
  // A freshly-rendered hero counter means a site just finished loading — the
  // most reliable signal to dismiss the loading overlay (no reliance on a
  // custom Shiny message, which doesn't always register in time).
  if (typeof smtLoadDone === "function") smtLoadDone();
  const target = parseFloat(el.getAttribute("data-target")) || 0;
  const suffix = el.dataset.suffix || "";          // e.g. "d", "m", "g"
  const isFloat = !Number.isInteger(target);
  const fmt = (v) => (isFloat ? v.toFixed(1) : Math.round(v).toLocaleString()) + suffix;
  // reduced-motion: snap to the final value, no animation
  if (window.matchMedia && window.matchMedia("(prefers-reduced-motion: reduce)").matches) {
    el.textContent = fmt(target); return;
  }
  const dur = 900;
  const start = performance.now();
  function tick(now) {
    const t = Math.min(1, (now - start) / dur);
    const eased = 1 - Math.pow(1 - t, 3); // easeOutCubic
    el.textContent = fmt(target * eased);
    if (t < 1) requestAnimationFrame(tick);
    else el.textContent = fmt(target);
  }
  requestAnimationFrame(tick);
}

function runCounters() {
  document.querySelectorAll(".count-up").forEach(animateCount);
}

// Re-run whenever Shiny injects fresh stat cards.
const heroObserver = new MutationObserver(() => runCounters());
document.addEventListener("DOMContentLoaded", function () {
  const host = document.body;
  heroObserver.observe(host, { childList: true, subtree: true });
  runCounters();
});

// ---- confetti on legendary / epic finds ----------------------------------
function rodentConfetti(big) {
  if (typeof confetti !== "function") return;
  // Desert Data Labs desert-night palette (azure blue / coral / gold + bright accents).
  const colors = ["#38a8e8", "#ffd24a", "#fb8a7e", "#7cc8f5", "#5cc6f5"];
  const burst = (opts) => confetti(Object.assign({ colors, disableForReducedMotion: true }, opts));
  burst({ particleCount: big ? 140 : 70, spread: big ? 100 : 70, origin: { y: 0.3 }, startVelocity: 42 });
  if (big) {
    setTimeout(() => burst({ particleCount: 80, angle: 60, spread: 70, origin: { x: 0 } }), 180);
    setTimeout(() => burst({ particleCount: 80, angle: 120, spread: 70, origin: { x: 1 } }), 320);
  }
  mascotCheer(big);
}

// ---- mascot celebration: a little mouse hops up + fades on a legendary/epic find
function mascotCheer(big) {
  try {
    if (window.matchMedia && window.matchMedia("(prefers-reduced-motion: reduce)").matches) return;
    var src = document.querySelector("#loadOverlay .mascot");
    if (!src) return;
    var wrap = document.createElement("div");
    wrap.className = "mascot-cheer";
    wrap.appendChild(src.cloneNode(true));
    document.body.appendChild(wrap);
    setTimeout(function () { if (wrap.parentNode) wrap.parentNode.removeChild(wrap); }, 1700);
  } catch (e) {}
}

// ---- delighters: restore-last-site + recents (ONE localStorage namespace) ----
// Two keys, both owned here: smtLastSite (the single code to auto-restore) and
// smtRecents (a JSON ring buffer of the last few codes, newest first). Both are
// READ once on shiny:connected and WRITTEN in
// exactly one place: the smt_save_site handler the server fires on every site
// load. Nothing else touches these keys.
function smtReadRecents() {
  try {
    var raw = localStorage.getItem("smtRecents");
    if (!raw) return [];
    var arr = JSON.parse(raw);
    return Array.isArray(arr) ? arr.filter(function (c) { return typeof c === "string" && c; }) : [];
  } catch (e) { return []; }
}
// Persist a just-loaded site: set smtLastSite, unshift into the recents ring
// (dedup, newest first, cap 4), and push the fresh ring back so the recents strip
// re-renders live this session. The server calls this via shinyjs::runjs on every
// successful load — a direct client eval, which is more reliable here than a
// custom-message handler. ONE place that writes these two keys.
function smtSaveSite(code) {
  if (!code) return;
  try {
    localStorage.setItem("smtLastSite", code);
    var ring = smtReadRecents().filter(function (c) { return c !== code; });
    ring.unshift(code);
    ring = ring.slice(0, 4);
    localStorage.setItem("smtRecents", JSON.stringify(ring));
    if (window.Shiny && Shiny.setInputValue)
      Shiny.setInputValue("smtRecents", JSON.stringify(ring), { priority: "event" });
  } catch (e) {}
}

// shiny:connected READ — hand the saved last-site + recents ring to the server so
// the startup resolver can auto-restore and the recents strip can render. This is
// bound at TOP LEVEL (document exists immediately; no Shiny needed to call
// addEventListener) so the listener is in place BEFORE Shiny's websocket connects.
// An EVENT-priority setInputValue sent before connect is dropped, and a listener
// registered after shiny:connected already fired would never run — binding here,
// early, is what makes the round-trip reliable (the resolver is once=TRUE, so the
// belt-and-suspenders retries below are harmless).
function smtPushStored() {
  if (!(window.Shiny && Shiny.setInputValue)) return;
  // Push the recents ring so the splash strip renders. The startup SITE restore is
  // resolved client-side (smtStartupResolve below) — it fires siteExplore after the
  // first render, the same proven path a map-dot tap uses, so the splash-hide lands
  // reliably (a server-side load during the initial flush lost shinyjs::hide).
  try { Shiny.setInputValue("smtRecents", JSON.stringify(smtReadRecents()), { priority: "event" }); }
  catch (e) { Shiny.setInputValue("smtRecents", "[]", { priority: "event" }); }
}
// In this Shiny build `shiny:connected` is a jQuery-triggered event that does NOT
// fire a native DOM event, so document.addEventListener("shiny:connected") never
// runs and jQuery may not exist at top level yet. The robust path: poll until the
// Shiny websocket is actually OPEN, then push the stored state exactly once. The
// startup resolver is once=TRUE, so this single push is all it needs.
(function smtAwaitConnected() {
  var tries = 0;
  var pushed = false;
  var t = setInterval(function () {
    var sock = window.Shiny && Shiny.shinyapp && Shiny.shinyapp.$socket;
    var open = sock && (sock.readyState === 1 || sock.readyState === undefined);
    if (open && window.Shiny.setInputValue) {
      if (!pushed) { pushed = true; smtPushStored(); }
      // one more push a beat later to clear any pre-flush drop, then stop
      setTimeout(smtPushStored, 300);
      clearInterval(t);
    } else if (++tries > 300) {   // ~30s safety cap
      clearInterval(t);
    }
  }, 100);
  // also push on the jQuery connected event once jQuery is available (future
  // reconnects), without depending on it for the initial restore.
  var jqTries = 0;
  var jt = setInterval(function () {
    if (window.jQuery) { clearInterval(jt); jQuery(document).on("shiny:connected", smtPushStored); }
    else if (++jqTries > 100) clearInterval(jt);
  }, 100);
})();

// ---- startup site restore (deep link ?site= > localStorage last-site) ----------
// Resolved ON THE CLIENT and dispatched through the SAME input a real map-dot tap
// fires (siteExplore), AFTER the first render has settled — so load_site_full() ->
// ingest()'s shinyjs::hide("splash") finds #splash in the DOM and the transition
// lands. (A server-side resolver calling load_site_full during the initial reactive
// flush ran before #splash existed, so the hide was silently lost and the site
// loaded *underneath* a still-visible splash.) Precedence: URL beats localStorage.
(function smtStartupResolve() {
  var done = false;
  function go() {
    if (done || !(window.Shiny && Shiny.setInputValue)) return;
    var target = "";
    try {
      var u = new URLSearchParams(window.location.search).get("site");
      target = (u && u.trim()) || (localStorage.getItem("smtLastSite") || "");
    } catch (e) {}
    if (!target) return;            // cold start -> splash stays (don't latch; allow a later retry)
    done = true;
    try { smtLoadStart(target + " — loading…"); } catch (e) {}
    Shiny.setInputValue("siteExplore", target, { priority: "event" });
  }
  // Fire after the first flush settles (DOM rendered, observers bound). jQuery
  // shiny:idle is the reliable post-render hook; a load-timer fallback covers a
  // missed event.
  var bound = setInterval(function () {
    if (window.jQuery) { clearInterval(bound); jQuery(document).one("shiny:idle", function () { setTimeout(go, 60); }); }
  }, 50);
  window.addEventListener("load", function () { setTimeout(go, 900); });
})();
// The shiny:connected READ binding that hands these keys to the server is
// registered inside the DOMContentLoaded + window.Shiny block below (alongside
// the smt_save_site write handler), NOT at top level — at top level `$`/`Shiny`
// may not exist yet, and a ReferenceError there would abort the rest of app.js.

// ---- loading overlay (opaque, indeterminate) -----------------------------
// A site load is one synchronous blocking call whose duration we can't know,
// so we show an INDETERMINATE animated bar (no fake %) on an OPAQUE backdrop —
// it just spins until the server signals it's done. No number to "stall" at,
// and you don't see half-rendered data through it.
var smtSafetyTimer = null;
function smtLoadStart(label) {
  var ov = document.getElementById("loadOverlay");
  if (!ov) return;
  // Raise the overlay IMMEDIATELY, synchronously, on the click. A site load is
  // 1–3s of BLOCKING work on the worker (decompress + clean + leaderboard + the
  // Overview tab's plotly renders). A server-sent "show" message can't paint
  // until that block ends — by then it's too late — so the only honest feedback
  // is to show it client-side right now. (Loads are never truly instant, so the
  // old 250ms defer just hid the feedback during exactly the freeze it's for.)
  var siteText = label || "";
  if (!siteText) {
    var sel = document.getElementById("site");
    if (sel && sel.options && sel.selectedIndex >= 0) siteText = sel.options[sel.selectedIndex].text;
  }
  var siteEl = document.getElementById("loadSite");
  if (siteEl) siteEl.textContent = siteText;
  ov.style.display = "flex";
  if (navigator.vibrate) { try { navigator.vibrate(12); } catch (e) {} }  // tactile "got it"
  clearTimeout(smtSafetyTimer);
  smtSafetyTimer = setTimeout(function () {  // safety net so it can never stick
    var note = document.querySelector(".load-note");
    if (note) note.textContent = "Still working — a large site or a slow NEON Portal can take a bit. You can close this and try again.";
    setTimeout(smtLoadDone, 5000);
  }, 90000);
}
function smtLoadDone() {
  clearTimeout(smtSafetyTimer);
  var ov = document.getElementById("loadOverlay");
  if (ov) ov.style.display = "none";
}

// (The site report card is now a server-side PDF streamed by a Shiny
//  downloadHandler — output$reportPdf, via the hero downloadLink — so the old
//  browser-print path (smtPrintReport) has been removed.)

// ---- save the dossier trading card as a PNG (html-to-image) --------------
function smtSaveCard() {
  var node = document.getElementById("smtCardNode");
  if (!node) return;
  // §2.15: never fail silently when the CDN-only capture lib didn't load (blocked
  // wifi / ad-blocker). The user tapped "Save card" — tell them why nothing happened.
  if (typeof htmlToImage === "undefined") {
    var _m = "A file the export needs didn't load — check your network or ad-blocker, then reload.";
    if (typeof Swal !== "undefined") Swal.fire({ icon: "error", title: "Couldn't save the card",
      text: _m, confirmButtonColor: "#1f78c4" });
    else window.alert(_m);
    return;
  }
  var name = (node.querySelector(".tc-id") || {}).textContent || "card";
  // skipFonts avoids html-to-image scanning cross-origin CDN stylesheets for
  // @font-face (which throws CORS errors); Rubik is already loaded on the page,
  // so the same-document canvas render still uses it.
  htmlToImage.toPng(node, { pixelRatio: 2, cacheBust: true, skipFonts: true })
    .then(function (dataUrl) {
      var a = document.createElement("a");
      a.download = "neon-mammal-" + name.replace(/[^A-Za-z0-9]+/g, "") + ".png";
      a.href = dataUrl;
      a.click();
    })
    .catch(function () {
      if (typeof Swal !== "undefined") Swal.fire({ icon: "error", title: "Couldn't save the card",
        text: "Try again, or screenshot it instead.", confirmButtonColor: "#1f78c4" });
    });
}

// ---- guided tour (driver.js) ---------------------------------------------
function smtTour() {
  if (!window.driver || !window.driver.js) return;
  var D = window.driver.js.driver;
  var steps = [
    { element: ".picker-mode", popover: { title: "Two ways in", side: "bottom",
        description: "Explore <b>by site</b> — tap a dot for its card — or switch to <b>by species</b> to map where one animal turns up across the country." } },
    { element: ".picker-map-wrap", popover: { title: "The national map", side: "top",
        description: "Every NEON site is a dot — <b>bigger</b> = more animals caught, <b>color</b> = the family of the most-common species there. Tap any dot to see its card, then choose <b>Explore</b> or <b>About</b>." } },
    { element: ".select-panel", popover: { title: "Or pick by name", side: "top",
        description: "Prefer a list? Pick a <b>state</b> and <b>site</b> here, set the <b>date window</b>, then tap <b>Explore this site</b>." } },
    { element: "#compareBtn", popover: { title: "Compare two sites", side: "top",
        description: "Put two sites head-to-head — species, diversity, and abundance, side by side." } }
  ].filter(function (s) { return document.querySelector(s.element); });
  if (!steps.length) return;
  var d = D({ showProgress: true, allowClose: true, steps: steps, popoverClass: "driverjs-theme",
    nextBtnText: "Next", prevBtnText: "Back", doneBtnText: "Got it" });
  d.drive();
}

// The tour is now MANUAL only (the "How it works" button), per the house
// no-auto-tour rule — an unrequested guided tour on first paint is intrusive.
// smtTour() above still runs on demand when the user asks for it.

// ---- dismiss any open info popover (click-outside + Esc) -----------------
// bslib/Bootstrap popovers don't close on an outside click by default, so make
// every "ⓘ" popover dismissible the way users expect.
function smtClosePopovers() {
  document.querySelectorAll(".popover").forEach(function (pop) {
    var trig = pop.id ? document.querySelector('[aria-describedby="' + pop.id + '"]') : null;
    if (trig && window.bootstrap && bootstrap.Popover) {
      var inst = bootstrap.Popover.getInstance(trig);
      if (inst) { inst.hide(); return; }
    }
    pop.remove(); // fallback: just remove the floating popover
  });
}
document.addEventListener("click", function (e) {
  if (e.target.closest(".popover") || e.target.closest(".info-dot") ||
      e.target.closest("bslib-popover")) return;        // clicking inside/trigger -> leave it
  if (document.querySelector(".popover")) smtClosePopovers();
});
document.addEventListener("keydown", function (e) {
  if (e.key === "Escape") smtClosePopovers();
});
// a11y: let Enter / Space activate any div styled as role="button" (the site-open
// chip, change-site, browse-all and any future custom control) just like a click.
document.addEventListener("keydown", function (e) {
  if (e.key !== "Enter" && e.key !== " ") return;
  var el = e.target.closest && e.target.closest('[role="button"]');
  if (el) { e.preventDefault(); el.click(); }
});

// ---- Shiny custom message handlers ---------------------------------------
// app.js is a <head> script and can execute BEFORE Shiny's own JS has defined
// `window.Shiny`, OR AFTER DOMContentLoaded has already fired. The original
// `DOMContentLoaded + if(window.Shiny)` guard could therefore miss BOTH windows
// and silently never register these handlers (countUp / loadDone / smtLoadStart /
// kickMaps / the delighter smt_save_site). whenShinyReady() removes the race: it
// polls until `window.Shiny` exists, then runs the registration exactly once.
function whenShinyReady(fn) {
  if (window.Shiny && Shiny.addCustomMessageHandler) { fn(); return; }
  var tries = 0;
  var t = setInterval(function () {
    if (window.Shiny && Shiny.addCustomMessageHandler) { clearInterval(t); fn(); }
    else if (++tries > 200) clearInterval(t);   // ~20s safety cap
  }, 100);
}
whenShinyReady(function () {
  {
    Shiny.addCustomMessageHandler("countUp", function (_msg) {
      // small delay so the freshly-rendered DOM is in place
      setTimeout(runCounters, 60);
    });
    Shiny.addCustomMessageHandler("confetti", function (msg) {
      rodentConfetti(msg && msg.big);
    });
    Shiny.addCustomMessageHandler("loadDone", function (_msg) { smtLoadDone(); });
    // (smtSaveSite — the localStorage write — is a top-level function the server
    //  calls via shinyjs::runjs, defined above; no custom handler needed here.
    //  The shiny:connected READ that delivers smtLastSite + smtRecents to the
    //  startup resolver is bound at TOP LEVEL — see smtBindConnectedRead() below —
    //  so it is registered BEFORE Shiny connects and never misses the event.)
    // server-triggered overlay (e.g. a click on the national picker map, which
    // has no inline onclick to call smtLoadStart directly)
    Shiny.addCustomMessageHandler("smtLoadStart", function (msg) {
      smtLoadStart(msg && msg.label);
    });
    // A Leaflet map that initialised inside a hidden tab/container (the Plot-map
    // tab, or the picker map re-shown after "change site") can paint blank until
    // it recomputes its size. Dispatching 'resize' makes every Leaflet map
    // invalidateSize. The server kicks this after re-showing the splash.
    // Fire across several frames: after "change site" re-shows the splash, the
    // page_fillable layout (and the relocated select-panel) needs a moment to
    // settle its width before Leaflet measures, or the map captures a half-width
    // and paints narrow. Multiple dispatches catch the settled layout.
    Shiny.addCustomMessageHandler("kickMaps", function (_msg) {
      var kick = function () { try { window.dispatchEvent(new Event("resize")); } catch (e) {} };
      requestAnimationFrame(kick);
      [80, 250, 500, 900].forEach(function (t) { setTimeout(kick, t); });
    });
  }
});

// Re-fit any Leaflet map the moment its tab becomes visible (hidden-init blank fix).
document.addEventListener("shown.bs.tab", function () {
  setTimeout(function () { try { window.dispatchEvent(new Event("resize")); } catch (e) {} }, 60);
});
