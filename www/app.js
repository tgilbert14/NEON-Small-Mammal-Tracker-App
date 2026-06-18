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
  // Desert Data Labs house palette (navy / cardinal / gold + warm accents).
  const colors = ["#0C234B", "#AB0520", "#FFD200", "#c9a300", "#2f7fb5"];
  const burst = (opts) => confetti(Object.assign({ colors, disableForReducedMotion: true }, opts));
  burst({ particleCount: big ? 140 : 70, spread: big ? 100 : 70, origin: { y: 0.3 }, startVelocity: 42 });
  if (big) {
    setTimeout(() => burst({ particleCount: 80, angle: 60, spread: 70, origin: { x: 0 } }), 180);
    setTimeout(() => burst({ particleCount: 80, angle: 120, spread: 70, origin: { x: 1 } }), 320);
  }
}

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
  if (!node || typeof htmlToImage === "undefined") return;
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
        text: "Try again, or screenshot it instead.", confirmButtonColor: "#0C234B" });
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
    { element: "#compareBtn", popover: { title: "Compare two sites", side: "top",
        description: "Put two sites head-to-head — species, diversity, and abundance, side by side." } },
    { element: "#demoBtn2", popover: { title: "In a hurry?", side: "top",
        description: "Jump straight into the Jornada desert demo — it opens instantly." } }
  ].filter(function (s) { return document.querySelector(s.element); });
  if (!steps.length) return;
  var d = D({ showProgress: true, allowClose: true, steps: steps, popoverClass: "driverjs-theme",
    nextBtnText: "Next", prevBtnText: "Back", doneBtnText: "Got it" });
  d.drive();
}

// auto-run once on a visitor's first time, after the picker map exists
function smtMaybeAutoTour() {
  try { if (localStorage.getItem("smtToured") === "1") return; } catch (e) { return; }
  var tries = 0;
  var iv = setInterval(function () {
    tries++;
    if (document.querySelector(".picker-map-wrap") && window.driver) {
      clearInterval(iv);
      try { localStorage.setItem("smtToured", "1"); } catch (e) {}
      setTimeout(smtTour, 700);
    } else if (tries > 30) { clearInterval(iv); }
  }, 400);
}
document.addEventListener("DOMContentLoaded", function () { smtMaybeAutoTour(); });

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

// ---- Shiny custom message handlers ---------------------------------------
document.addEventListener("DOMContentLoaded", function () {
  if (window.Shiny) {
    Shiny.addCustomMessageHandler("countUp", function () {
      // small delay so the freshly-rendered DOM is in place
      setTimeout(runCounters, 60);
    });
    Shiny.addCustomMessageHandler("confetti", function (msg) {
      rodentConfetti(msg && msg.big);
    });
    Shiny.addCustomMessageHandler("loadDone", function () { smtLoadDone(); });
    // server-triggered overlay (e.g. a click on the national picker map, which
    // has no inline onclick to call smtLoadStart directly)
    Shiny.addCustomMessageHandler("smtLoadStart", function (msg) {
      smtLoadStart(msg && msg.label);
    });
    // A Leaflet map that initialised inside a hidden tab/container (the Plot-map
    // tab, or the picker map re-shown after "change site") can paint blank until
    // it recomputes its size. Dispatching 'resize' makes every Leaflet map
    // invalidateSize. The server kicks this after re-showing the splash.
    Shiny.addCustomMessageHandler("kickMaps", function () {
      setTimeout(function () { try { window.dispatchEvent(new Event("resize")); } catch (e) {} }, 90);
    });
  }
});

// Re-fit any Leaflet map the moment its tab becomes visible (hidden-init blank fix).
document.addEventListener("shown.bs.tab", function () {
  setTimeout(function () { try { window.dispatchEvent(new Event("resize")); } catch (e) {} }, 60);
});
