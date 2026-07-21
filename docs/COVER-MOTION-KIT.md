# Cover & Motion — Starting Kit (DDL NEON suite)

> **This is a STARTING POINT, not a spec.** Every app's cover begins from the shared archetype below and
> then **diverges by subject**. The published motion mockups and any placeholder copy/art are **rough
> outlines to be tested live and rewritten per project** — never ship the placeholder words or a hand-drawn
> stand-in illustration. Adapt freely; keep only the non-negotiable floor (last section).
>
> **For subagents:** read this before building or revamping a cover. It gives you somewhere to start; the
> owner tunes it per project. When a refinement proves out, promote it to the canonical `TG-Data-Apps`
> playbook via `curator` (see `neonize-playbook.md` §9).

## Two loved references (the archetype, already shipped)

Both covers use ONE template with different skins — that IS the model.

- **Small Mammal** (`docs/index.html`) — ground `#111512` + **acid `#dce319`** + ember `#e87531` + paper
  `#f3e8cb`; hook **"Who moves / *after dark?*"**; screenprint of a mouse at an oversized box-trap. CTA
  "Meet the mammals". *Poster-led.*
- **Vegetation** (`../NEON-Vegetation-Structure-Explorer/docs/index.html`) — ground `#0c1f16` + lichen
  `#a8d98d` + **gold `#f0bd4f`** + bark `#c7754e` + paper `#f5f0df`; hook **"Tagged. Measured. / *Still
  changing.*"**; screenprint of a tagged tree with a diameter tape. CTA "Pick a place". *Poster-led.*

There are also two rough **motion mockups** published as owner Artifacts (the "Design DNA" and the "Motion
& AI Cover" pages) — treat them as sketches of the *treatment*, not finished covers.

## The shared bones (start here — keep the structure)

- `.poster` full-height dark ground + a radial **corner glow** (the app's accent) + a **`feTurbulence`
  grain** overlay (`mix-blend-mode: soft-light`, opacity ~.16) — the screenprint feel.
- `.topline`: **Desert Data Labs** brand + mark (left); one **suite jump → Driver Cascade** (right).
- `.poster-grid`: `minmax(360px,.84fr) minmax(520px,1.36fr)` — **copy left, art right (art is larger)**.
- Copy: uppercase accent **eyebrow** (`<PRODUCT> · unofficial`) → huge **serif hook**
  (`clamp(~4rem, 7.3vw, ~7.6rem)`, `line-height:.82`, `letter-spacing:-.065em`, **one line in the app's
  accent**) → a one-line **promise** (≤12 words) → **one primary CTA** into the Connect app.
- Art: a **screenprint illustration** of the subject, full-height, `object-fit:cover`, **bleeding left**
  into the dark via `linear-gradient(90deg, ground → transparent)`, with an **honesty art-note**
  ("Editorial illustration — not a field photo or data record").
- Footer (paper ground): **DPID + CC BY 4.0 + "unofficial, not endorsed"**, a `What am I looking at?`
  honesty `<details>`, and Source / Feedback links.
- Responsive at 980 / 700 / 420 / 340; the art stacks on top on phones.

## What each app SWAPS (its own skin — no two alike)

Accent(s) · the hook words + which word carries the accent · the screenprint art + its subject · the CTA
label · the eyebrow/brand accent · the honesty caption. **Choose a distinct bold accent per app** (acid,
gold, …) — there is **no shared "house" accent**; the family reads through the *structure*, not the color.

## The motion layer (bring the poster alive — all reduced-motion-safe)

Movement is the default, but it must collapse to a clean, beautiful **static** cover under
`prefers-reduced-motion: reduce`.

**This is the LIGHT tier — `neonize`'s base.** Parallax, scroll-reveals, and a scroll-cued mood on a poster
cover are app-side (this kit). For the DEEP immersive tier — a scroll-scrubbed camera flight, a
fly-through-the-world scene — `neonize` CALLS the canonical **Mithril** guild + the `scroll-world` /
`scroll-film` skills (the Higgsfield scroll-scrub pipeline); don't rebuild that here.

- **Parallax hero** — the art (and copy) drift on scroll + pointer, by depth, on `requestAnimationFrame`
  (passive scroll listener; transforms only, no layout thrash).
- **Art with depth** — the subject art as a framed, slightly-tilted card with a big soft drop-shadow (+ a
  faint accent glow), floating **top-right**; or keep the full-bleed image — both read well.
- **Scroll reveals** — content blocks fade/slide in via `IntersectionObserver`, staggered.
- **Scroll-cued mood** — a glow/tint or the palette shifts with scroll progress (e.g. a dawn glow rising
  through the hero).
- **Hover micro-interactions** — the CTA fills + lifts; cards lift.

## Motion options to prototype (owner ideas — test per app)

- **Video higher.** The AI cover loop can live UP in the hero (behind or beside the hook), not only lower
  down the page.
- **Scroll-video / scrollytelling.** A `<video>` (scroll-scrubbed) or a `<canvas>` whose content is driven
  by scroll progress — a scene that **changes or conveys the app's story as you scroll** (a tree growing,
  night falling, a plot filling in with tagged individuals). Prototype it and judge per app; keep a static
  poster fallback and honour reduced-motion.

## The AI asset pipeline — tools we own (use these)

**Owner's toolchain (2026-07): ChatGPT Pro (20×) + a Higgsfield account.** Default to what Pro already
covers; do not assume paid extras.

1. **Still + graphics → ChatGPT Pro / GPT-image (PRIMARY).** The screenprint hero art and the scroll-video
   first frame. Prompt in the app's DNA (ready prompts in the appendix below).
2. **Motion → Sora, included in ChatGPT Pro (PRIMARY).** Image-to-video on the GPT-image still: a slow
   ambient loop, or the scroll-video source. No extra cost on Pro.
3. **Motion → Higgsfield (SECONDARY, use sparingly).** Better stylized motion control, but credit-limited
   (only ~9 credits left as of 2026-07) — reserve for a deliberate A/B vs Sora, not routine passes.
4. **Integration → Claude wires it.** Poster-first `<video muted loop playsinline poster="…"
   preload="none">`; budget < ~2.5 MB; lazy-load; `prefers-reduced-motion` holds the still. **A perfect
   loop is NOT required from the model** — a plain 10–20s clip is fine; Claude makes it loop cleanly in
   code (short crossfade / opacity dissolve at the loop point). **Log every AI asset in
   `docs/IMAGE-PROVENANCE.md` with source/date/prompt/hash; AI art is NEVER relabelled as NEON data.**

## Per-app divergence (be creative — the cover follows the subject)

The poster archetype is perfect for a **front-door cover**, but the format should follow what the app *is*:

- **Poster-led** (Small Mammal) — a screenprint hero.
- **Immersive / scroll-led** (Vegetation) — a "growth" scroll where structure builds as you go; lead with
  the tap-to-pin Forest Size Lab.
- **Data-led** (e.g. Mosquito Pulse) — the hero *is* a live chart or interaction.
- **Systemic / map-led** (Driver-Cascade, the ambassador) — a constellation of the suite, or an animated
  weather→response cascade.

Same DNA, different personality. **Don't template one layout onto every app** — the owner explicitly does
not want them to look the same.

## The floor (non-negotiable, even in a rough draft)

Honesty art-note on the illustration + the `What am I looking at?` details · a detection index is never a
population · AI art labelled and never relabelled as data · `prefers-reduced-motion` honoured · legible
contrast on both grounds · visible focus ring · 44px touch targets · every asset logged in
`IMAGE-PROVENANCE.md`. Validate at desktop / 390 / 320, and record the exact Pages artifact + Connect
deployment commit before calling a cover shipped (`neonize-playbook.md` §7, the Cover Contract).

## Ready prompts + how an agent uses them

**How an agent uses this (Claude or Codex):** you can't call ChatGPT/Sora/Higgsfield yourself — so when a
cover needs art or motion, **hand the owner the exact prompt below** (tuned to the app's DNA), let them
generate the still (GPT-image) and clip (Sora) in ChatGPT Pro, then **wire the returned asset** into the
cover (poster-first video, reduced-motion fallback) and log it in `IMAGE-PROVENANCE.md`. Prompt → owner →
wire. Don't block on it: build the cover with the SVG/CSS stand-in first, drop the real asset in when it lands.

**Small Mammal — GPT-image (the screenprint still):**
> Bold flat editorial screenprint poster illustration, WPA / national-park style. Scene: a small nocturnal
> desert mouse pausing at the mouth of an oversized metal Sherman box live-trap, under a starry night sky
> with a warm gold moon low on the horizon; layered dune silhouettes and a saguaro. Palette limited to deep
> near-black green (#111512), acid chartreuse-yellow (#dce319) as the single bold accent, warm ember orange
> (#e87531), and cream (#f3e8cb). Flat confident shapes, high contrast, visible screenprint grain, dramatic
> but graphic — not photorealistic, no text, no watermark. Compose the mouse and trap toward the lower area
> with generous dark sky above for a headline. (Aspect: 4:5 portrait for a top-right art card, or 3:4 for a
> full-height right-column bleed.)

**Small Mammal — Sora (image-to-video, ambient loop from that still):**
> Animate this poster still with subtle, calm ambient motion for a website cover background: stars twinkle
> and drift very slowly, a faint haze rolls low over the dunes, the moon glow gently breathes, the mouse
> gives one small whisker twitch. Minimal slow camera, a barely-perceptible parallax push-in. Keep the exact
> flat screenprint style and palette; add or change nothing; no text. 8–12 seconds, gentle, start and end on
> a similar frame (loop-friendly), muted.

**Reusable template (any app):**
> STILL: "Bold flat editorial screenprint poster, WPA style. Scene: <app subject>. Palette: <app ground> +
> its ONE bold accent + 1–2 supports. Flat shapes, high contrast, screenprint grain, no text, no
> photorealism. <aspect>."
> MOTION: "Animate this still with subtle calm ambient motion suited to <subject> (leaves stirring / water
> rippling / stars drifting); keep the exact style and palette, add nothing, no text; 8–12s, slow,
> loop-friendly, muted."

---
*Starting kit v0.1 — grew from the Small Mammal + Vegetation Living Posters and the owner's motion
direction. A rough outline to build from and **alter per project**; enforce the frame with
`check_cover.mjs` / `check_in_app_landing.mjs`, and promote proven refinements to canonical
`TG-Data-Apps` via `curator`.*
