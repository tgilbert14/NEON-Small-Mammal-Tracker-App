# env_corr_pvalue() Type I calibration (2026-10-22)

Command: `Rscript --vanilla scripts/measure_env_null_calibration.R 400 500`
(branch `finish/sm-science`, after the unrounded observed-statistic fix).

Setup: the response is a synthetic series independent of every driver,
Gaussian white noise (rho 0) or AR(1) with rho 0.8, generated on the full
monthly calendar between the site's first and last sampled month and kept only
on the months the site actually sampled (its real gap pattern). Drivers are the
site's real monthly env bundle. Each run is the app's test: best abs(r) over
every driver x lag 0-12, circular-shift null with nperm = 500. 400 runs per
cell; the binomial SE of a true 0.05 rate is 0.011.

| site | sampled months (of span) | rho | P(p < 0.05) | P(p < 0.02) |
|---|---|---|---|---|
| SRER | 41 of 103 | 0.0 | 0.068 | 0.018 |
| SRER | 41 of 103 | 0.8 | 0.100 | 0.055 |
| HARV | 64 of 136 | 0.0 | 0.083 | 0.037 |
| HARV | 64 of 136 | 0.8 | 0.050 | 0.025 |
| JORN | 36 of 114 | 0.0 | 0.090 | 0.055 |
| JORN | 36 of 114 | 0.8 | 0.120 | 0.055 |

Reading: at a nominal 0.05 the test rejects 5 to 12% of the time with no real
link, so it is anti-conservative on gappy series, worst at the shortest (JORN)
and with strong autocorrelation. The rotation runs over the sampled-month
vector, so a shift moves values across seasonal gaps and does not keep the
real-time autocorrelation the observed series has. At a 0.02 gate the false
rate is 1.8 to 5.5%, at or below about 0.05 in every cell.

Recommendation for the lead: gate the app's verdict at p < 0.02 (that keeps
the realized false-positive rate near 5% across these three gap patterns), or
keep 0.05 and describe the p as approximate. A null that rotates in calendar
time would be the cleaner long-term fix.
