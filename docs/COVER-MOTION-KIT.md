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

## The AI asset pipeline (ChatGPT + Higgsfield)

- **ChatGPT / GPT-image → the still.** Prompt in the app's DNA: *"flat bold WPA-poster screenprint,
  <subject>, <palette>, limited palette, high contrast, no text, no photorealism, 3:2."* This is the
  screenprint art (or the scroll-video's first frame).
- **Higgsfield → the motion.** Image-to-video on that still: a subtle seamless loop, or the scroll-video
  source. Export muted webm + mp4.
- **Integration (Claude wires it).** Poster-first `<video muted loop playsinline poster="…" preload="none">`;
  budget < ~2.5 MB; lazy-load; a `prefers-reduced-motion` guard holds the still. **Log every AI asset in
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

---
*Starting kit v0.1 — grew from the Small Mammal + Vegetation Living Posters and the owner's motion
direction. A rough outline to build from and **alter per project**; enforce the frame with
`check_cover.mjs` / `check_in_app_landing.mjs`, and promote proven refinements to canonical
`TG-Data-Apps` via `curator`.*
