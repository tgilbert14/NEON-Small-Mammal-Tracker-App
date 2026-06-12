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
  const isFloat = !Number.isInteger(target);
  const dur = 900;
  const start = performance.now();
  function tick(now) {
    const t = Math.min(1, (now - start) / dur);
    const eased = 1 - Math.pow(1 - t, 3); // easeOutCubic
    const val = target * eased;
    el.textContent = isFloat ? val.toFixed(1) : Math.round(val).toLocaleString();
    if (t < 1) requestAnimationFrame(tick);
    else el.textContent = isFloat ? target.toFixed(1) : Math.round(target).toLocaleString();
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
  const colors = ["#2dd4bf", "#f5a524", "#ffd24a", "#c879ff", "#4ab8ff"];
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
var smtShowTimer = null, smtSafetyTimer = null;
function smtLoadStart() {
  var ov = document.getElementById("loadOverlay");
  if (!ov) return;
  // defer ~250ms so INSTANT (bundled) loads never even flash the overlay
  clearTimeout(smtShowTimer);
  smtShowTimer = setTimeout(function () {
    var sel = document.getElementById("site");
    var siteText = "";
    if (sel && sel.options && sel.selectedIndex >= 0) siteText = sel.options[sel.selectedIndex].text;
    var siteEl = document.getElementById("loadSite");
    if (siteEl) siteEl.textContent = siteText;
    ov.style.display = "flex";
    clearTimeout(smtSafetyTimer);
    smtSafetyTimer = setTimeout(function () {  // safety net so it can never stick
      var note = document.querySelector(".load-note");
      if (note) note.textContent = "Still working — a large site or a slow NEON Portal can take a bit. You can close this and try again.";
      setTimeout(smtLoadDone, 5000);
    }, 90000);
  }, 250);
}
function smtLoadDone() {
  clearTimeout(smtShowTimer);
  clearTimeout(smtSafetyTimer);
  var ov = document.getElementById("loadOverlay");
  if (ov) ov.style.display = "none";
}

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
  }
});
