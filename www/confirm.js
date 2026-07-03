/* confirm.js — the "How it works" help dialog (SweetAlert2) */
$(document).ready(function () {
  // delegated so it works even though the button is rendered inside the sidebar
  $(document).on("click", "#help", function () {
    // SweetAlert2 is a CDN lib; on blocked school/corp wifi or with an ad-blocker
    // it never loads. Unguarded, this click throws `Swal is not defined` and the
    // ONLY onboarding affordance dies with a console error. Fall back to a plain
    // dialog so the help content still reaches the user. (Real fix: vendor the lib
    // into www/ with a CDN onerror fallback — see the FABLE deploy-armor note.)
    if (typeof Swal === "undefined") {
      window.alert(
        "How this works\n\n" +
        "1. The map opens to all 46 NEON sites. Tap one to dive in.\n" +
        "2. Start on the Overview to meet the species; tap any top stat for a ranking.\n" +
        "3. The Hall of Fame ranks every individual — click one to open its Dossier.\n" +
        "4. Dig into measurements, the Chonk Index, its trap-grid home range & replay.\n\n" +
        "RARITY (by total captures): Legendary 15+, Epic 10–14, Rare 6–9, Uncommon 3–5, Common 1–2.\n" +
        "CHONK INDEX: an adult weight percentile within species (50 = typical, 95+ = megachonk).\n\n" +
        "Data: NEON Small Mammal Box Trapping (DP1.10072.001)."
      );
      return;
    }
    Swal.fire({
      title: "🐾 How this works",
      width: 640,
      html:
        "<div style='text-align:left;font-size:14px;line-height:1.7;color:#cfe0f5'>" +
        "<b>1.</b> The map opens to all 46 NEON sites. Tap one to dive in.<br>" +
        "<b>2.</b> Start on the <b>Overview</b> to meet the species; tap any top stat for a ranking<br>" +
        "<b>3.</b> The <b>Hall of Fame</b> ranks every individual — click one to open its <b>Dossier</b><br>" +
        "<b>4.</b> Dig into measurements, the <b>Chonk Index</b>, its trap-grid home range &amp; replay" +
        "<hr style='border-color:rgba(255,255,255,.14);margin:14px 0'>" +
        "<div style='color:#7cc8f5;font-weight:700;margin-bottom:6px'>RARITY — set by total captures</div>" +
        "<div style='display:grid;grid-template-columns:auto 1fr;gap:4px 12px;align-items:center'>" +
        keyRow("#ffd24a", "⭐ Legendary", "15+ captures") +
        keyRow("#fb8a7e", "💎 Epic", "10–14 captures") +
        keyRow("#38a8e8", "🔵 Rare", "6–9 captures") +
        keyRow("#5fb56a", "🟢 Uncommon", "3–5 captures") +
        keyRow("#9fb0cf", "▫ Common", "1–2 captures") +
        "</div>" +
        "<div style='color:#7cc8f5;font-weight:700;margin:14px 0 6px'>CHONK INDEX</div>" +
        "<div style='color:#cfe0f5'>An honest <b>adult weight percentile within species</b> — 50 is a perfectly typical adult, 95+ is a MEGACHONK. The dossier's body-size map shows the real weight-vs-size picture.</div>" +
        "<div style='color:#9fb0cf;margin-top:12px;font-size:12px'>📊 Data: live from the NEON <b>Small Mammal Box Trapping</b> product (DP1.10072.001). Metrics reviewed against Peig &amp; Green (2009), Krebs (1966) &amp; Gotelli &amp; Colwell (2001) — see the <b>About</b> tab.</div>" +
        "</div>",
      background: "#0e1d40",
      color: "#eaf2ff",
      focusConfirm: false,
      confirmButtonText: "Let's go",
      confirmButtonColor: "#1f78c4"
    });
  });

  function keyRow(color, label, desc) {
    return "<div style='font-weight:700;color:" + color + ";white-space:nowrap'>" + label + "</div>" +
           "<div style='color:#9fb0cf'>" + desc + "</div>";
  }
});
