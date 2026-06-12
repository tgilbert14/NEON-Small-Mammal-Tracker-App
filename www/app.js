/* =========================================================================
   app.js — count-up stat counters + celebratory confetti
   ========================================================================= */

// ---- animated count-up for the hero stat band ----------------------------
function animateCount(el) {
  if (el.dataset.animated === "1") return;
  el.dataset.animated = "1";
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

// ---- full-screen loading overlay with a creeping percent bar -------------
// The NEON download is one opaque blocking call, so we animate an estimated
// progress bar client-side (starts instantly on click) and snap to 100% when
// the server signals it's done.
var smtLoadTimer = null;
function smtLoadStart() {
  var ov = document.getElementById("loadOverlay");
  if (!ov) return;
  var sel = document.getElementById("site");
  var siteText = "";
  if (sel && sel.options && sel.selectedIndex >= 0) siteText = sel.options[sel.selectedIndex].text;
  var siteEl = document.getElementById("loadSite");
  if (siteEl) siteEl.textContent = siteText;
  var fill = document.getElementById("loadFill"), pct = document.getElementById("loadPct");
  var p = 0;
  if (fill) fill.style.width = "0%";
  if (pct) pct.textContent = "0%";
  ov.style.display = "flex";
  clearInterval(smtLoadTimer);
  smtLoadTimer = setInterval(function () {
    var step = Math.max(0.35, (93 - p) * 0.035);   // ease out toward ~93%
    p = Math.min(93, p + step);
    if (fill) fill.style.width = p.toFixed(0) + "%";
    if (pct) pct.textContent = p.toFixed(0) + "%";
  }, 200);
}
function smtLoadDone() {
  var ov = document.getElementById("loadOverlay");
  if (!ov) return;
  clearInterval(smtLoadTimer);
  var fill = document.getElementById("loadFill"), pct = document.getElementById("loadPct");
  if (fill) fill.style.width = "100%";
  if (pct) pct.textContent = "100%";
  setTimeout(function () { ov.style.display = "none"; }, 500);
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
