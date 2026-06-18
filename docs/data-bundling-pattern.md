# The "bundle + .rds durable store" pattern

A reusable recipe for making a data app **instant, offline-capable, and cheap to host** when its
data comes from a slow or rate-limited source (an API, a scraper, a big remote DB). Used here to
turn ~1-minute live NEON downloads into instant loads. Carry it to any future project.

---

## The core idea (in one sentence)

> Pre-compute the data **once** into small, compressed files committed alongside the app, have the
> app **read those files at runtime**, and "update" by **re-running the build script and
> redeploying** — instead of querying a live source (or standing up a database) on every request.

The files *are* the database. They're read-only, versioned with the code, and shipped in the deploy
bundle.

---

## What `.rds` is and why it's the right container here

`.rds` is R's native **binary serialization** of a single object (here, a data frame).

- **Exact fidelity** — column types, factors, dates, attributes all survive a round-trip. No
  re-parsing, no "is this column character or numeric?" guessing like with CSV.
- **Fast** — `readRDS()` is a binary load; far faster than parsing CSV/JSON.
- **Compresses hard** — `saveRDS(x, file, compress = "xz")` shrinks tabular data dramatically.
  In this app a site went from an ~80k-row raw table to **~0.4 MB** on disk.
- **One object per file** — perfect for "one file per entity" (one site, one team, one customer…).

Python equivalents: `parquet` (best, columnar, cross-language) or `pickle`/`feather`. Same pattern,
different container. Use **parquet** if anything non-R will ever read the files.

---

## Why not just a live API call every time?

- **Slow** — users wait on every load; a bad first impression.
- **Fragile** — the upstream source can be down, rate-limited, or change its schema.
- **Repeated work** — the same query runs thousands of times for data that barely changes.

## Why not a real database (SQLite/Postgres)?

Often overkill, and on ephemeral hosting (shinyapps.io, most PaaS) a DB you write at runtime
**doesn't survive restarts/scale events**. A persistent DB means a separate server + connection
management + a sync job. Worth it only when you need: writes from users, very large data that can't
be bundled, complex multi-table joins at query time, or live freshness. For **read-only, slowly-
changing, bundle-sized** data, the file bundle wins on simplicity.

**Rule of thumb:** if the whole dataset (trimmed + compressed) fits comfortably in your deploy
bundle (tens of MB), bundle it. If it's gigabytes or needs live writes, use a real DB.

---

## The build script (the "refresh")

A standalone script (`scripts/refresh_data.R` here) that you run on your machine, not in the app:

```r
keep <- c(...the columns the app actually uses...)        # 1. TRIM — drop unused columns
for (entity in entities) {
  out <- file.path("data/sites", paste0(entity, ".rds"))
  if (file.exists(out)) next                              # 2. RESUMABLE — skip done work
  raw <- tryCatch(fetch(entity), error = function(e) NULL) # 3. ROBUST — one failure ≠ whole job dies
  if (is.null(raw)) next
  saveRDS(raw[, keep], out, compress = "xz")               # 4. TRIM + COMPRESS to disk
}
```

Four habits that make it pleasant:

1. **Trim to the columns you use** — the single biggest size win (NEON gives 72 columns; the app
   needs ~33).
2. **Resumable** — skip files that already exist, so a flaky 40-minute download you can just re-run.
3. **Robust per item** — wrap each fetch in `tryCatch`; log and skip failures.
4. **Compress** — `compress = "xz"` (slowest write, smallest file — fine for a build step).

"**Pull newer data if needed**" = delete the file(s) you want fresh and re-run (existing files are
skipped), then redeploy. You can even schedule this (a GitHub Action / cron) to re-bundle monthly.

---

## The app side (read + graceful fallback)

```r
load_bundle <- function(id) {                    # read the file if it exists, else NULL
  f <- file.path("data/sites", paste0(id, ".rds"))
  if (file.exists(f)) readRDS(f) else NULL
}

# in the load handler:
b <- load_bundle(id)
if (!is.null(b)) {
  use(filter_window(b, start, end))              # INSTANT — from disk, filtered in-memory
} else {
  use(fetch_live(id, start, end))                # FALLBACK — only when not bundled
}
```

Two ideas that make it robust:

- **Filter on read.** Bundle the *full* record per entity; apply the user's date/window filter
  in-memory after reading. One file serves every window.
- **Graceful fallback.** If a file is missing (not yet bundled, or a brand-new entity), fall through
  to the live source. The app works the same; it's just slower for un-bundled items. So you can ship
  the code before the bundle is complete.

---

## Deploy: the files travel with the app

The deploy bundles the `.rds` files alongside the code (here `scripts/deploy.R` adds
`data/sites/*.rds` to the file list). Because they're committed to the repo, they're versioned with
the code and reproducible. Keep raw downloads out of the bundle (`.gitignore` the neon cache /
`filesToProcess`) — only the trimmed `.rds` ship.

### ⚠ Rebuilt bundles do NOT go live until you republish

On a git-backed host (Posit Connect Cloud), the running app serves the **published snapshot**, and
`manifest.json` pins a **SHA/MD5 checksum per bundled file**. So rebuilding a `.rds` locally changes
nothing in production, and a changed bundle whose checksum wasn't refreshed can even fail the deploy.
This bit us once — bundles looked updated locally but the live app kept serving the old data until a
republish. The required sequence after any data rebuild:

1. rebuild the bundles (`scripts/refresh_data.R`)
2. **regenerate the manifest** so its checksums match the new files (`scripts/write_manifest.R` →
   `rsconnect::writeManifest()`)
3. `git add data/ manifest.json && git commit`
4. **push + republish** on Connect Cloud (git-backed redeploy)

Miss step 2 or 4 and the deployed app silently keeps the stale data. (On a non-manifest host like
shinyapps.io, there's no checksum step, but you still must redeploy — the bundle only updates on push.)

---

## When to reach for this

| Situation | Use this pattern? |
| --- | --- |
| Slow/rate-limited API, data changes slowly, read-only | ✅ Yes — ideal |
| Dataset (trimmed+compressed) fits in tens of MB | ✅ Yes |
| Users need to write/save data | ❌ No — needs a real DB |
| Data is gigabytes, or must be live-fresh | ❌ No — DB / warehouse / live query |
| Cross-language consumers | ✅ but use **parquet**, not `.rds` |

---

## TL;DR recipe for a future project

1. Write a `refresh` script: loop entities → fetch → **trim columns** → `saveRDS(..., compress="xz")`
   into `data/<thing>/<id>.rds`. Make it **resumable** + **tryCatch per item**.
2. App: `load_bundle(id)` reads the file; **filter in memory**; **fall back** to live if missing.
3. Commit the `.rds` files (they're the durable store) and include them in the deploy bundle.
4. To refresh: delete the stale files, re-run the script, **regenerate the manifest, commit, and
   republish** — on a git-backed/manifest host the live app keeps the old data until you do.
