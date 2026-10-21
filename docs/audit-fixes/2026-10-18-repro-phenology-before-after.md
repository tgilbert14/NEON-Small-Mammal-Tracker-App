# Reproductive phenology before/after the flag_repro() token fix (2026-10-18)

`flag_repro()` in `R/helpers.R` used substring matching (`grepl`), so
`"not pregnant"` matched `pregnant`, `"nonenlarged"` matched `enlarged`, and
`"not plugged"` matched `plugged`, while the estrous token `"swollen"` was never
matched. Nearly every adult female scored as reproductive, so the Phenology
tab's female curve sat at 92 to 100% in every month. The fix matches trimmed,
lowercased tokens exactly against the NEON.DOC.000481vQ vocabulary.

`pf` is the percent of sexed adult females (distinct tags per month) recorded
pregnant, lactating, or vaginally receptive. `pm` is the percent of adult males
recorded scrotal. A month with fewer than 5 animals of that sex reads NA. `pm`
does not change, because the old male test already excluded `nonscrotal`.

Token values in the committed bundles (SRER, ABBY, HARV): `pregnancyStatus` is
stored as `not` / `pregnant` / `unknown` / NA (not `not pregnant`); `vagina`
uses `both`, `neither`, `not plugged`, `plugged`, `swollen`, `unknown`.

## Commands

Before (the pre-fix helpers from base commit 7c0a59b, saved inside `R/` so
`chdir = TRUE` sourcing resolves its relative paths):

```sh
git show 7c0a59b:R/helpers.R > R/zz_helpers_orig_tmp.R
Rscript --vanilla scripts/measure_repro_phenology.R R/zz_helpers_orig_tmp.R
rm R/zz_helpers_orig_tmp.R
```

After:

```sh
Rscript --vanilla scripts/measure_repro_phenology.R
```

## Before (substring matching)

```text
SRER
 mon females  pf males pm
 Jan       0  NA     0 NA
 Feb       0  NA     0 NA
 Mar     226 100   296 74
 Apr      81 100    98 86
 May     434 100   476 84
 Jun     327 100   360 85
 Jul     489  99   523 72
 Aug      71 100    77 64
 Sep     291  99   249 62
 Oct     416  99   398 45
 Nov     234  98   217 18
 Dec     107  92    93 48

ABBY
 mon females  pf males pm
 Jan       0  NA     0 NA
 Feb       0  NA     0 NA
 Mar       0  NA     0 NA
 Apr      46  93    54  4
 May       7 100    15 33
 Jun     298  97   310 54
 Jul     278  99   273 62
 Aug     390  97   344 35
 Sep     274 100   193 16
 Oct       0  NA     0 NA
 Nov       0  NA     0 NA
 Dec       0  NA     0 NA

HARV
 mon females  pf males pm
 Jan       0  NA     0 NA
 Feb       0  NA     0 NA
 Mar       0  NA     0 NA
 Apr      44  98    23 22
 May     177  97   109 55
 Jun     547  98   473 56
 Jul     609  99   742 54
 Aug    1012  97  1128 57
 Sep     828  94   722 44
 Oct     362 100   262 15
 Nov       0  NA     0 NA
 Dec       0  NA     0 NA
```

## After (exact-token matching)

```text
SRER
 mon females pf males pm
 Jan       0 NA     0 NA
 Feb       0 NA     0 NA
 Mar     226 41   296 74
 Apr      81 64    98 86
 May     434 76   476 84
 Jun     327 74   360 85
 Jul     489 69   523 72
 Aug      71 72    77 64
 Sep     291 57   249 62
 Oct     416 47   398 45
 Nov     234 22   217 18
 Dec     107 36    93 48

ABBY
 mon females pf males pm
 Jan       0 NA     0 NA
 Feb       0 NA     0 NA
 Mar       0 NA     0 NA
 Apr      46 17    54  4
 May       7 43    15 33
 Jun     298 67   310 54
 Jul     278 72   273 62
 Aug     390 57   344 35
 Sep     274 37   193 16
 Oct       0 NA     0 NA
 Nov       0 NA     0 NA
 Dec       0 NA     0 NA

HARV
 mon females pf males pm
 Jan       0 NA     0 NA
 Feb       0 NA     0 NA
 Mar       0 NA     0 NA
 Apr      44 68    23 22
 May     177 51   109 55
 Jun     547 49   473 56
 Jul     609 36   742 54
 Aug    1012 35  1128 57
 Sep     828 43   722 44
 Oct     362 35   262 15
 Nov       0 NA     0 NA
 Dec       0 NA     0 NA
```

ABBY May (7 females) is above the n >= 5 gate but is still a thin month.
