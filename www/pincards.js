/* =========================================================================
   pincards.js — tap-to-pin cards + export-with-pins for the Size Lab scatter.

   Ported from the Big 12 Girth Index pin-card system (ggiraph) onto PLOTLY:
   the same draggable/resizable navy card + gold leader line + html-to-image
   "download with the pins baked in" — but the click is detected with plotly's
   native `plotly_click` event and the card HTML rides in each point's
   `customdata`, so there's no second rendering stack (the app stays plotly).

   The pins, the leader-line SVG layer, and the gold anchor dots all live INSIDE
   the chart's `.smt-pinnable` box (not the plot div), so they travel with the
   box and — crucially — get captured when html-to-image snapshots the box.

   Anchors are stored as DATA coordinates (not frozen pixels), so they reposition
   correctly on resize / fullscreen / relayout. The plotly_click handler is
   RE-attached on every (re)render: a Shiny+plotly re-render runs purge+newPlot
   on the SAME div, which silently wipes gd.on() listeners — so a one-time bind
   (the old __smtPinBound guard) left the chart dead after the first re-render.
   ========================================================================= */
(function () {
  "use strict";
  var NS = "http://www.w3.org/2000/svg";

  function boxOf(node) { return node ? node.closest(".smt-pinnable") : null; }
  function bgColor() {
    var dark = document.documentElement.getAttribute("data-bs-theme") === "dark" ||
               document.body.getAttribute("data-bs-theme") === "dark";
    return dark ? "#16213a" : "#ffffff";
  }

  /* map a data point (dx, dy) to box-relative pixels via plotly's live axes, so
     a pin's leader line follows the dot through any resize/relayout. */
  function anchorPx(gd, box, dx, dy) {
    var fl = gd && gd._fullLayout;
    if (!fl || !fl.xaxis || !fl.yaxis || typeof fl.xaxis.l2p !== "function") return null;
    var gdR = gd.getBoundingClientRect(), boxR = box.getBoundingClientRect();
    return {
      ax: fl.xaxis.l2p(dx) + fl.xaxis._offset + (gdR.left - boxR.left),
      ay: fl.yaxis.l2p(dy) + fl.yaxis._offset + (gdR.top - boxR.top)
    };
  }

  function linesLayer(box) {
    var s = box.querySelector(":scope > svg.smt-pin-lines");
    if (!s) {
      s = document.createElementNS(NS, "svg");
      s.setAttribute("class", "smt-pin-lines");
      box.appendChild(s);
    }
    return s;
  }
  /* x2/y2 chase the card centre (honours its scale() transform via getBoundingClientRect) */
  function updateLine(pin) {
    if (!pin.__line) return;
    var r = pin.getBoundingClientRect(), b = pin.__box.getBoundingClientRect();
    pin.__line.setAttribute("x2", r.left - b.left + r.width / 2);
    pin.__line.setAttribute("y2", r.top - b.top + r.height / 2);
  }

  window.smtClearPins = function () {
    document.querySelectorAll(".smt-pin").forEach(function (p) { p.remove(); });
    document.querySelectorAll("svg.smt-pin-lines").forEach(function (s) { s.innerHTML = ""; });
  };

  /* re-anchor every pin's leader line + dot from its stored DATA coords */
  function repositionAll(gd, box) {
    box.querySelectorAll(".smt-pin").forEach(function (pin) {
      if (pin.__dx == null || !pin.__line) return;
      var a = anchorPx(gd, box, pin.__dx, pin.__dy);
      if (!a) return;
      pin.__line.setAttribute("x1", a.ax); pin.__line.setAttribute("y1", a.ay);
      if (pin.__dot) { pin.__dot.setAttribute("cx", a.ax); pin.__dot.setAttribute("cy", a.ay); }
      updateLine(pin);
    });
  }

  function makePin(gd, box, html, dx, dy, key) {
    var bR = box.getBoundingClientRect();
    var a = anchorPx(gd, box, dx, dy) || { ax: bR.width / 2, ay: bR.height / 2 };
    var pin = document.createElement("div");
    pin.className = "smt-pin";
    pin.__box = box; pin.__key = key; pin.__dx = dx; pin.__dy = dy;
    pin.innerHTML = "<button class='smt-pin-close' title='Close'>&times;</button>" + html;
    pin.querySelectorAll("em.smt-pin-hint").forEach(function (em) {
      var br = em.previousElementSibling;
      if (br && br.tagName === "BR") br.remove();
      em.remove();
    });
    var grip = document.createElement("div");
    grip.className = "smt-pin-resize";
    grip.title = "Drag to resize · double-tap to reset";
    pin.appendChild(grip);

    /* position the card near the dot, clamped on BOTH axes so the close button
       and resize grip always stay inside the box (and inside an exported PNG) */
    pin.style.left = Math.max(4, Math.min(a.ax + 24, bR.width - 250)) + "px";
    pin.style.top = Math.max(4, Math.min(a.ay + 14, bR.height - 150)) + "px";
    box.appendChild(pin);

    var layer = linesLayer(box);
    var ln = document.createElementNS(NS, "line");
    ln.setAttribute("x1", a.ax); ln.setAttribute("y1", a.ay);
    ln.setAttribute("stroke", "#FFD200"); ln.setAttribute("stroke-width", "2.5");
    ln.setAttribute("stroke-linecap", "round");
    layer.appendChild(ln);
    var dot = document.createElementNS(NS, "circle");
    dot.setAttribute("cx", a.ax); dot.setAttribute("cy", a.ay); dot.setAttribute("r", "4.5");
    dot.setAttribute("fill", "#FFD200"); dot.setAttribute("stroke", "#0C234B");
    dot.setAttribute("stroke-width", "1.5");
    layer.appendChild(dot);
    pin.__line = ln; pin.__dot = dot;
    updateLine(pin);

    pin.querySelector(".smt-pin-close").addEventListener("click", function () {
      ln.remove(); dot.remove(); pin.remove();
    });

    /* drag-to-move (clamped so a fat-thumb drag can't fling the card off-box) */
    pin.addEventListener("pointerdown", function (ev) {
      if (ev.target.closest("a, .smt-pin-close, .smt-open, .smt-pin-resize")) return;
      ev.preventDefault();
      try { pin.setPointerCapture(ev.pointerId); } catch (e) {}
      var sx = ev.clientX - pin.offsetLeft, sy = ev.clientY - pin.offsetTop;
      function mv(em) {
        var nb = pin.__box.getBoundingClientRect();
        pin.style.left = Math.max(4, Math.min(em.clientX - sx, nb.width - 40)) + "px";
        pin.style.top = Math.max(4, Math.min(em.clientY - sy, nb.height - 28)) + "px";
        updateLine(pin);
      }
      function up() { pin.removeEventListener("pointermove", mv); pin.removeEventListener("pointerup", up); }
      pin.addEventListener("pointermove", mv);
      pin.addEventListener("pointerup", up);
    });

    /* grip-to-resize via scale() (clientX throughout, matching drag, so a
       scrolling mobile viewport can't make the scale jump) */
    grip.addEventListener("pointerdown", function (ev) {
      ev.preventDefault(); ev.stopPropagation();
      try { grip.setPointerCapture(ev.pointerId); } catch (e) {}
      var startX = ev.clientX, startScale = pin.__scale || 1;
      var baseW = pin.getBoundingClientRect().width / startScale;
      function mv(em) {
        var s = Math.min(2.4, Math.max(0.5, startScale + (em.clientX - startX) / baseW));
        pin.__scale = s; pin.style.transform = "scale(" + s + ")"; updateLine(pin);
      }
      function up() { grip.removeEventListener("pointermove", mv); grip.removeEventListener("pointerup", up); }
      grip.addEventListener("pointermove", mv);
      grip.addEventListener("pointerup", up);
    });
    grip.addEventListener("dblclick", function () {
      pin.__scale = 1; pin.style.transform = ""; updateLine(pin);
    });
    return pin;
  }

  function handleClick(gd, box, d) {
    if (!d || !d.points || !d.points.length) return;
    var pt = d.points[0];
    var html = pt.customdata;
    if (!html) return;                 // only points carrying a card (skip the fit line)
    var key = (html.match(/data-tag='([^']+)'/) || [])[1] || html.slice(0, 48);
    var dup = Array.prototype.find.call(box.querySelectorAll(".smt-pin"),
      function (p) { return p.__key === key; });
    if (dup) { dup.classList.remove("smt-pulse"); void dup.offsetWidth; dup.classList.add("smt-pulse"); return; }
    /* anchor from the DATA point (not the finger): exact, and touch-safe — on
       touch devices plotly's synthesized event has no usable clientX/Y */
    makePin(gd, box, html, pt.x, pt.y, key);
  }

  /* a cheap signature of the plotted DATA — changes when a filter changes (species
     traces / different points), but NOT on a resize, and NOT when the gold
     "★ tracking" highlight trace is appended. Opening a QC card from a pin sets the
     tracked individual, which appends that diamond (N -> N+1 traces); without
     excluding it the signature would flip and bindPlot would wipe every pin the
     instant the user opened a card from one — the documented happy path. Real
     filter changes still alter the species traces' x, captured by the terms below. */
  function dataSig(gd) {
    if (!gd.data || !gd.data.length) return "";
    var core = gd.data.filter(function (t) {
      return !(t.name && t.name.indexOf("tracking") !== -1);
    });
    if (!core.length) return "";
    var x = core[0].x || [];
    return core.length + ":" + x.length + ":" + x[0] + ":" + x[x.length - 1];
  }

  /* (re)bind a plotly graph div. Safe to call repeatedly — it removes any prior
     handler first. Clears pins when the underlying DATA changed (filter / select). */
  function bindPlot(gd) {
    if (!gd || typeof gd.on !== "function") return;
    var box = boxOf(gd); if (!box) return;

    var sig = dataSig(gd);
    if (gd.__smtSig !== undefined && gd.__smtSig !== sig) {
      box.querySelectorAll(".smt-pin").forEach(function (p) { p.remove(); });
      var ll = box.querySelector("svg.smt-pin-lines"); if (ll) ll.innerHTML = "";
    }
    gd.__smtSig = sig;

    if (gd.__smtClick && gd.removeListener) { try { gd.removeListener("plotly_click", gd.__smtClick); } catch (e) {} }
    gd.__smtClick = function (d) { handleClick(gd, box, d); };
    gd.on("plotly_click", gd.__smtClick);

    if (gd.__smtRelayout && gd.removeListener) { try { gd.removeListener("plotly_relayout", gd.__smtRelayout); } catch (e) {} }
    gd.__smtRelayout = function () { repositionAll(gd, box); };
    gd.on("plotly_relayout", gd.__smtRelayout);

    if (!box.__smtRO && window.ResizeObserver) {
      box.__smtRO = new ResizeObserver(function () {
        var g = box.querySelector(".js-plotly-plot"); if (g) repositionAll(g, box);
      });
      box.__smtRO.observe(box);
    }
  }

  function scan() {
    document.querySelectorAll(".smt-pinnable .js-plotly-plot").forEach(bindPlot);
  }
  /* rAF-coalesced scan so the document-wide observer doesn't run scan() on every
     count-up / DT / plotly mutation (it fires in bursts during animations) */
  var scanPending = false;
  function scanSoon() {
    if (scanPending) return;
    scanPending = true;
    requestAnimationFrame(function () { scanPending = false; scan(); });
  }

  /* ---- exports (with a toast + double-fire guard) ------------------------- */
  function toastStart(msg) {
    if (typeof Swal === "undefined") return;
    Swal.fire({ toast: true, position: "top-end", title: msg, showConfirmButton: false,
      allowOutsideClick: false, didOpen: function () { if (Swal.showLoading) Swal.showLoading(); } });
  }
  function toastDone(msg, isErr) {
    if (typeof Swal === "undefined") return;
    Swal.fire({ toast: true, position: "top-end", icon: isErr ? "error" : "success",
      title: msg, showConfirmButton: false, timer: 2200 });
  }
  function downloadUrl(url, name) {
    var a = document.createElement("a"); a.download = name; a.href = url; a.click();
  }
  var saving = false;
  function snap(node, name, beforeEl) {
    if (!node || typeof htmlToImage === "undefined" || saving) return;
    saving = true;
    // force the plotly chart to its current size first (a tab that rendered while
    // hidden, or a just-toggled fullscreen, can leave a 0-sized / stale SVG)
    var gd = node.querySelector ? node.querySelector(".js-plotly-plot") : null;
    if (gd && window.Plotly && Plotly.Plots) { try { Plotly.Plots.resize(gd); } catch (e) {} }
    node.querySelectorAll(".smt-pin.smt-pulse").forEach(function (p) { p.classList.remove("smt-pulse"); });
    toastStart("Rendering image…");
    setTimeout(function () {
      htmlToImage.toPng(node, { pixelRatio: 2, backgroundColor: bgColor(), cacheBust: true, skipFonts: true })
        .then(function (url) { downloadUrl(url, name); toastDone("Saved ✓"); })
        .catch(function () { toastDone("Render failed — try again", true); })
        .then(function () { saving = false; });
    }, 90);
  }
  window.smtSaveScatter = function () { snap(document.querySelector(".smt-pinnable"), "neon-bodysize-lab.png"); };
  window.smtSaveQcCard = function () {
    var node = document.getElementById("qcCardNode");
    if (!node) return;
    var short = node.getAttribute("data-short") || "qc";
    snap(node, "neon-qc-" + short.replace(/[^A-Za-z0-9]+/g, "") + ".png");
  };

  /* tap (or keyboard-activate) a highlighted "Open QC history card" chip inside a
     pinned card -> ask the server to select that individual + render its QC card */
  function openChip(el) {
    if (!el || !window.Shiny) return;
    var tag = el.getAttribute("data-tag");
    if (tag) Shiny.setInputValue("qcCardRequest", tag, { priority: "event" });
  }
  document.addEventListener("click", function (e) {
    var el = e.target.closest(".smt-open");
    if (el) openChip(el);
  });
  document.addEventListener("keydown", function (e) {
    if (e.key !== "Enter" && e.key !== " ") return;
    var el = e.target.closest && e.target.closest(".smt-open");
    if (el) { e.preventDefault(); openChip(el); }
  });

  document.addEventListener("DOMContentLoaded", function () {
    var mo = new MutationObserver(scanSoon);
    mo.observe(document.body, { childList: true, subtree: true });
    scan();
  });
  /* re-bind/re-fit when the Size Lab tab becomes visible (plotly in a hidden tab
     can render late, and a tab-show dispatches a resize that relayouts the plot) */
  document.addEventListener("shown.bs.tab", function () { setTimeout(scan, 80); });

  /* the server (qcCardRequest observer) fires "smtRevealQc" after selecting the
     individual; bring its QC card into view (it re-renders async via uiOutput, so
     a short settle delay) — otherwise on a phone the card materializes below the
     540px chart and the chip reads as "did nothing". Honors reduced-motion.
     Registered LAST and fully guarded so it can never block the pin-binding
     listeners above; self-polls because this IIFE runs at <head> before Shiny exists. */
  (function registerReveal() {
    try {
      if (window.Shiny && Shiny.addCustomMessageHandler) {
        Shiny.addCustomMessageHandler("smtRevealQc", function () {
          var reduce = window.matchMedia &&
            window.matchMedia("(prefers-reduced-motion: reduce)").matches;
          // Scroll the QC card into view. NB the #qcHistoryCard uiOutput wrapper is
          // display:contents (bslib fill layout) — it has NO box, so scrolling to it
          // is a no-op. Target the actual rendered card (#qcCardNode), or the empty
          // state (.qc-empty), once it exists AND has laid out (height > 1). The card
          // re-renders async after pick_individual(), so poll briefly (~1.8s) instead
          // of firing once on a fixed delay (the earlier single-shot fired too early).
          var tries = 0;
          (function go() {
            var n = document.getElementById("qcCardNode") || document.querySelector(".qc-empty");
            if (n && n.getBoundingClientRect().height > 1) {
              n.scrollIntoView({ behavior: reduce ? "auto" : "smooth", block: "start" });
              return;
            }
            if (++tries < 30) setTimeout(go, 60);
          })();
        });
        return;
      }
    } catch (e) { return; }   // never let a duplicate/late registration kill binding
    setTimeout(registerReveal, 60);
  })();
})();
