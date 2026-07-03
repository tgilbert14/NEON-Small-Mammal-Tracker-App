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
    return dark ? "#0e1d40" : "#ffffff";
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
    // a11y (WCAG 4.1.2): the &times; glyph is not a reliable accessible name, so
    // label the button explicitly; title stays for the mouse-hover tooltip.
    pin.innerHTML = "<button class='smt-pin-close' type='button' title='Close card' aria-label='Close card'>&times;</button>" + html;
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
    ln.setAttribute("stroke", "#ffd24a"); ln.setAttribute("stroke-width", "2.5");
    ln.setAttribute("stroke-linecap", "round");
    layer.appendChild(ln);
    var dot = document.createElementNS(NS, "circle");
    dot.setAttribute("cx", a.ax); dot.setAttribute("cy", a.ay); dot.setAttribute("r", "4.5");
    dot.setAttribute("fill", "#ffd24a"); dot.setAttribute("stroke", "#070d1f");
    dot.setAttribute("stroke-width", "1.5");
    layer.appendChild(dot);
    pin.__line = ln; pin.__dot = dot;
    updateLine(pin);

    pin.querySelector(".smt-pin-close").addEventListener("click", function () {
      ln.remove(); dot.remove(); pin.remove();
    });

    /* drag-to-move (clamped so a fat-thumb drag can't fling the card off-box).
       A 4px move threshold (S8): a near-miss TAP — e.g. a few px off the QC chip —
       never engages the drag or preventDefaults, so the intended click still fires.
       move/up/cancel bound on WINDOW + capture released (S7): a pointerup off the
       card (scroll-steal, edge swipe, pointercancel) can never leave it stuck. */
    pin.addEventListener("pointerdown", function (ev) {
      if (ev.target.closest("a, .smt-pin-close, .smt-open, .smt-pin-resize")) return;
      var sx = ev.clientX - pin.offsetLeft, sy = ev.clientY - pin.offsetTop;
      var startX = ev.clientX, startY = ev.clientY, dragging = false;
      function mv(em) {
        if (!dragging) {
          if (Math.abs(em.clientX - startX) < 4 && Math.abs(em.clientY - startY) < 4) return;
          dragging = true;
          try { pin.setPointerCapture(ev.pointerId); } catch (e) {}
        }
        em.preventDefault();
        var nb = pin.__box.getBoundingClientRect();
        pin.style.left = Math.max(4, Math.min(em.clientX - sx, nb.width - 40)) + "px";
        pin.style.top = Math.max(4, Math.min(em.clientY - sy, nb.height - 28)) + "px";
        updateLine(pin);
      }
      function up() {
        window.removeEventListener("pointermove", mv);
        window.removeEventListener("pointerup", up);
        window.removeEventListener("pointercancel", up);
        try { pin.releasePointerCapture(ev.pointerId); } catch (e) {}
      }
      window.addEventListener("pointermove", mv);
      window.addEventListener("pointerup", up);
      window.addEventListener("pointercancel", up);
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
      function up() {
        window.removeEventListener("pointermove", mv);
        window.removeEventListener("pointerup", up);
        window.removeEventListener("pointercancel", up);
        try { grip.releasePointerCapture(ev.pointerId); } catch (e) {}
      }
      window.addEventListener("pointermove", mv);
      window.addEventListener("pointerup", up);
      window.addEventListener("pointercancel", up);
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
    /* fold EVERY core trace's point count + endpoints into the signature, not just
       core[0]: a filter change that drops a later species but leaves the first
       (alphabetical) trace's length + endpoints unchanged would otherwise slip
       through and strand pins on points that no longer exist. Widening the sig can
       only clear MORE stale pins, never fewer — and the "tracking" diamond is still
       excluded, so opening a QC card from a pin never trips it (the documented path). */
    return core.length + "#" + core.map(function (t) {
      var x = t.x || [];
      return x.length + "|" + x[0] + "|" + x[x.length - 1];
    }).join(",");
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
    // KILL-1: Swal ships from the SAME CDN as html-to-image, so on a blocked
    // network both are undefined together — an error toast would go MUTE in the
    // exact outage it exists for. Fall back to a native alert for errors.
    if (typeof Swal === "undefined") { if (isErr) window.alert(msg); return; }
    Swal.fire({ toast: true, position: "top-end", icon: isErr ? "error" : "success",
      title: msg, showConfirmButton: false, timer: 2200 });
  }
  function downloadUrl(url, name) {
    var a = document.createElement("a"); a.download = name; a.href = url; a.click();
  }
  /* chrome that must NEVER bake into an exported PNG: a pin's close button and
     resize grip live INSIDE .smt-pinnable (they're children of .smt-pin, which
     is a box child), so html-to-image renders them unless the filter drops them.
     smt-pin-hint is stripped in makePin() before it ever renders, but is listed
     for defence-in-depth — this is the single shared HIDE list, used by snap(). */
  var HIDE = ["smt-pin-close", "smt-pin-resize", "smt-pin-hint", "swal2-container"];
  function chromeFilter(n) {
    return !(n.classList && HIDE.some(function (c) { return n.classList.contains(c); }));
  }
  var saving = false;
  function snap(node, name, beforeEl) {
    if (saving) return;
    if (!node) return;
    // §2.15: a CDN-only capture lib is `undefined` on blocked school/corp wifi or
    // behind an ad-blocker. NEVER return silently — the user tapped "Download" and
    // deserves to know why nothing happened, not a dead button.
    if (typeof htmlToImage === "undefined") {
      toastDone("Download needs a file that didn't load — check your network or ad-blocker, then reload.", true);
      return;
    }
    saving = true;
    // force the plotly chart to its current size first (a tab that rendered while
    // hidden, or a just-toggled fullscreen, can leave a 0-sized / stale SVG)
    var gd = node.querySelector ? node.querySelector(".js-plotly-plot") : null;
    if (gd && window.Plotly && Plotly.Plots) { try { Plotly.Plots.resize(gd); } catch (e) {} }
    node.querySelectorAll(".smt-pin.smt-pulse").forEach(function (p) { p.classList.remove("smt-pulse"); });
    toastStart("Rendering image…");
    setTimeout(function () {
      htmlToImage.toPng(node, { pixelRatio: 2, backgroundColor: bgColor(), cacheBust: true, skipFonts: true, filter: chromeFilter })
        .then(function (url) { downloadUrl(url, name); toastDone("Saved ✓"); })
        .catch(function () { toastDone("Render failed — try again", true); })
        .then(function () { saving = false; });
    }, 90);
  }
  /* §1.13: a deterministic, self-describing filename. Scrape the SITE code + year
     window off the hero (the same scope rv$ctx stamps on-plot), so two exports
     from two different sites don't collide on one generic name in Downloads.
     Falls back to "" when the hero isn't mounted (nothing loaded yet), and the
     caller's base name still gives a valid, non-colliding file. */
  function clean(s) { return (s || "").replace(/[^A-Za-z0-9]+/g, "-").replace(/^-+|-+$/g, ""); }
  function snapSlug() {
    // KILL-2: prefer the site code stamped at the source (data-site — immune to
    // label-format drift); else the 4-letter NEON code in the hero label (skip the
    // "NEON" domain token); else the leading token as a last resort.
    var lab = document.querySelector(".hero-site-label");
    var rng = document.querySelector(".hero-site-range");
    var site = "";
    if (lab) {
      var codes = (lab.textContent.match(/\b[A-Z]{4}\b/g) || []).filter(function (c) { return c !== "NEON"; });
      site = lab.getAttribute("data-site") || codes[0] || lab.textContent.trim().split(/[\s—·-]/)[0] || "";
    }
    // reduce a range like "Jan 2022 – Dec 2024" to its bounding years
    var yrs = "";
    if (rng) {
      var m = (rng.textContent.match(/\d{4}/g) || []);
      if (m.length) yrs = m[0] === m[m.length - 1] ? m[0] : m[0] + "-" + m[m.length - 1];
    }
    return [clean(site), yrs].filter(Boolean).join("-");
  }
  function snapName(facet, extra) {
    var parts = ["neon", facet, snapSlug(), extra].filter(Boolean);
    return parts.join("-") + ".png";
  }
  window.smtSaveScatter = function () {
    snap(document.querySelector(".smt-pinnable"), snapName("bodysize"));
  };
  window.smtSaveQcCard = function () {
    var node = document.getElementById("qcCardNode");
    if (!node) return;
    var short = clean(node.getAttribute("data-short") || "");
    snap(node, snapName("qc", short));
  };

  /* Scroll the QC card into view once it renders. Hard-won points:
     • The #qcHistoryCard uiOutput wrapper is display:contents (bslib fill layout) —
       no box, so scrolling IT is a no-op. Scroll the actual rendered card
       (#qcCardNode), polling ~2.5s for it (it re-renders async after the server
       selects the individual).
     • behavior:"auto" (instant), NOT "smooth": the scroll happens in a NESTED bslib
       container (the window itself doesn't scroll — scrollHeight == clientHeight) and
       smooth scrollIntoView silently no-ops there. Instant is reliable + respects
       reduced-motion by definition.
     Driven from the chip click (below) — client-side, no dependence on a server
     round-trip message — so it can't silently no-op. */
  function revealQcCard() {
    var tries = 0;
    (function go() {
      var n = document.getElementById("qcCardNode");
      if (n && n.getBoundingClientRect().height > 1) {
        n.scrollIntoView({ behavior: "auto", block: "start" });
        return;
      }
      if (++tries < 50) setTimeout(go, 50);
    })();
  }

  /* tap (or keyboard-activate) a highlighted "Open QC history card" chip inside a
     pinned card -> select that individual server-side + scroll its QC card into view */
  function openChip(el) {
    if (!el || !window.Shiny) return;
    var tag = el.getAttribute("data-tag");
    if (!tag) return;
    Shiny.setInputValue("qcCardRequest", tag, { priority: "event" });
    revealQcCard();
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
        // Backup path: the chip click (openChip) already scrolls client-side; the
        // server also fires this after selecting the individual. Same reliable
        // instant-scroll-to-#qcCardNode helper.
        Shiny.addCustomMessageHandler("smtRevealQc", function () { revealQcCard(); });
        return;
      }
    } catch (e) { return; }   // never let a duplicate/late registration kill binding
    setTimeout(registerReveal, 60);
  })();
})();
