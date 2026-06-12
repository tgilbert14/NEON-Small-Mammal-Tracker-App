/* confirm.js — the "How it works" help dialog (SweetAlert2) */
$(document).ready(function () {
  // delegated so it works even though the button is rendered inside the sidebar
  $(document).on("click", "#help", function () {
    Swal.fire({
      title: "🐾 How this works",
      width: 640,
      html:
        "<div style='text-align:left;font-size:14px;line-height:1.7;color:#344049'>" +
        "<b>1.</b> The app opens on a live <b>demo</b> (Jornada, NM) — explore right away<br>" +
        "<b>2.</b> Start on the <b>Overview</b> to meet the species; tap any top stat for a ranking<br>" +
        "<b>3.</b> The <b>Hall of Fame</b> ranks every individual — click one to open its <b>Dossier</b><br>" +
        "<b>4.</b> Dig into measurements, the <b>Chonk Index</b>, its trap-grid home range &amp; replay" +
        "<hr style='border-color:#e2e7df;margin:14px 0'>" +
        "<div style='color:#1b6051;font-weight:700;margin-bottom:6px'>RARITY — set by total captures</div>" +
        "<div style='display:grid;grid-template-columns:auto 1fr;gap:4px 12px;align-items:center'>" +
        keyRow("#d99000", "⭐ Legendary", "15+ captures") +
        keyRow("#7c3aed", "💎 Epic", "10–14 captures") +
        keyRow("#2563eb", "🔵 Rare", "6–9 captures") +
        keyRow("#16a34a", "🟢 Uncommon", "3–5 captures") +
        keyRow("#6b7280", "▫ Common", "1–2 captures") +
        "</div>" +
        "<div style='color:#1b6051;font-weight:700;margin:14px 0 6px'>CHONK INDEX</div>" +
        "<div style='color:#344049'>An honest <b>adult weight percentile within species</b> — 50 is a perfectly typical adult, 95+ is a MEGACHONK. The dossier's body-size map shows the real weight-vs-size picture.</div>" +
        "<div style='color:#6b7a85;margin-top:12px;font-size:12px'>📊 Data: live from the NEON <b>Small Mammal Box Trapping</b> product (DP1.10072.001). Metrics reviewed against Peig &amp; Green (2009), Krebs (1966) &amp; Gotelli &amp; Colwell (2001) — see the <b>About</b> tab.</div>" +
        "</div>",
      background: "#ffffff",
      color: "#1f2a30",
      focusConfirm: false,
      confirmButtonText: "Let's go",
      confirmButtonColor: "#1b6051"
    });
  });

  function keyRow(color, label, desc) {
    return "<div style='font-weight:700;color:" + color + ";white-space:nowrap'>" + label + "</div>" +
           "<div style='color:#6b7a85'>" + desc + "</div>";
  }
});
