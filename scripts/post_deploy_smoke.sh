#!/usr/bin/env bash
# post_deploy_smoke.sh — post-deploy liveness check for the refresh loop.
#
# After an intentional review merge to main, Connect Cloud republishes the app.
# This closes the loop: it confirms the LIVE surfaces actually come back up. It is
# cold-start aware — a Connect Cloud worker can take a minute-plus to wake, so each
# URL is polled with backoff for up to ~5 minutes before it is declared down.
#
# Usage:  post_deploy_smoke.sh "<label>=<url>" ["<label>=<url>" ...]
# Exit 0 if every URL returns a healthy status within the retry budget; 1 otherwise.
# The Connect app must also return the app-specific UI marker and must not match a
# known host error page. This is semantic startup evidence, not an HTTP-only probe.
# The app surface must also carry <html lang="en">, the app <title>, and a
# <meta name="ddl-build"> whose digest matches the checked-out tree, so a green
# run proves the live worker serves the pushed revision (not just any revision).
# On failure the post-deploy workflow opens/updates a GitHub issue; on recovery it
# closes the matching open outage issue (see post-deploy.yml).

set -uo pipefail

MAX_ATTEMPTS="${SMOKE_MAX_ATTEMPTS:-10}"   # ~10 tries
SLEEP_BASE="${SMOKE_SLEEP_BASE:-5}"        # 5s,10s,15s... capped — ~5 min total
CONNECT_TIMEOUT="${SMOKE_CONNECT_TIMEOUT:-15}"
MAX_TIME="${SMOKE_MAX_TIME:-45}"
APP_MARKER="${SMOKE_APP_MARKER:-ddl-app-ready}"

REPO_ROOT="${SMOKE_REPO_ROOT:-$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)}"
REQUIRE_BUILD="${SMOKE_REQUIRE_BUILD:-1}"     # 0 = skip the revision check (local probing)
APP_TITLE="${SMOKE_APP_TITLE:-<title>NEON Small Mammal Tracker</title>}"
APP_LANG='<html lang="en">'

# Expected build digests for the checked-out tree, mirroring build_digest() in
# global.R. Canonical text = one "<path>\t<md5>" line per file, sorted by path
# (byte order), joined by "\n" with no trailing newline; the digest is its md5.
#   m-: lines from manifest.json's files map (path + recorded checksum)
#   f-: lines from md5 over the runtime files (global/ui/server.R, R/*.R, www/**,
#       data/**/*.rds, data/*.json, data-sample/**; hidden names skipped)
# The live app must report one of the two; which one depends on whether Connect
# ships manifest.json into the running directory.
compute_expected_builds() {
  node - "$REPO_ROOT" <<'NODE'
const fs = require("node:fs");
const path = require("node:path");
const crypto = require("node:crypto");
const root = process.argv[2];
const md5 = (buf) => crypto.createHash("md5").update(buf).digest("hex");
const digest = (pairs) => md5(Buffer.from(
  pairs.sort((a, b) => (a[0] < b[0] ? -1 : a[0] > b[0] ? 1 : 0))
    .map(([p, c]) => `${p}\t${c}`).join("\n"), "utf8"));
const out = [];
try {
  const man = JSON.parse(fs.readFileSync(path.join(root, "manifest.json"), "utf8"));
  const files = man.files || {};
  const keys = Object.keys(files);
  if (keys.length) out.push("m-" + digest(keys.map((k) => [k, String(files[k].checksum || "").toLowerCase()])));
} catch (e) { /* no readable manifest: only the file method applies */ }
const walk = (dir, recursive, keep) => {
  const abs = path.join(root, dir);
  if (!fs.existsSync(abs)) return [];
  let found = [];
  for (const ent of fs.readdirSync(abs, { withFileTypes: true })) {
    if (ent.name.startsWith(".")) continue;
    const rel = `${dir}/${ent.name}`;
    if (ent.isDirectory()) { if (recursive) found = found.concat(walk(rel, true, keep)); }
    else if (keep(ent.name)) found.push(rel);
  }
  return found;
};
const any = () => true;
let runtime = ["global.R", "ui.R", "server.R"]
  .concat(walk("R", false, (n) => /\.R$/.test(n)))
  .concat(walk("www", true, any))
  .concat(walk("data", true, (n) => /\.rds$/.test(n)))
  .concat(walk("data", false, (n) => /\.json$/.test(n)))
  .concat(walk("data-sample", true, any));
runtime = [...new Set(runtime)].filter((p) => fs.existsSync(path.join(root, p)));
out.push("f-" + digest(runtime.map((p) => [p, md5(fs.readFileSync(path.join(root, p)))])));
console.log(out.join(" "));
NODE
}

EXPECTED_BUILDS="${SMOKE_EXPECTED_BUILDS:-}"
if [[ "$REQUIRE_BUILD" == "1" && -z "$EXPECTED_BUILDS" ]]; then
  if ! EXPECTED_BUILDS=$(compute_expected_builds) || [[ -z "$EXPECTED_BUILDS" ]]; then
    echo "could not compute the expected build digest from $REPO_ROOT" >&2
    exit 2
  fi
fi
[[ "$REQUIRE_BUILD" == "1" ]] && echo "expected ddl-build (either): $EXPECTED_BUILDS"

# 0 when the body carries the exact ddl-build meta for one expected digest.
has_expected_build() {
  local body="$1" want
  for want in $EXPECTED_BUILDS; do
    grep -Fq "name=\"ddl-build\" content=\"$want\"" "$body" && return 0
  done
  return 1
}

fail=0
report=""

check_one() {
  local label="$1" url="$2" attempt code body nap
  body=$(mktemp)
  for ((attempt = 1; attempt <= MAX_ATTEMPTS; attempt++)); do
    # Follow redirects and retain the body. Posit host error pages can return 200,
    # so status alone is never enough for the app surface.
    code=$(curl -sS -o "$body" -w '%{http_code}' -L \
                --connect-timeout "$CONNECT_TIMEOUT" --max-time "$MAX_TIME" \
                -A 'ddl-uptime-smoke/1.0' "$url" 2>/dev/null || echo "000")
    if [[ "$code" =~ ^(2|3)[0-9][0-9]$ ]]; then
      if grep -Eqi 'startup error|application failed to start|application error|service unavailable' "$body"; then
        echo "  wait [$label] $url -> $code but body is a host error page (attempt $attempt/$MAX_ATTEMPTS)"
      elif [[ "$label" == *"app"* ]] && ! grep -Fq "$APP_MARKER" "$body"; then
        echo "  wait [$label] $url -> $code but app-ready marker is absent (attempt $attempt/$MAX_ATTEMPTS)"
      elif [[ "$label" == *"app"* ]] && ! { grep -Fq "$APP_TITLE" "$body" && grep -Fq "$APP_LANG" "$body"; }; then
        echo "  wait [$label] $url -> $code but app title or html lang is absent (attempt $attempt/$MAX_ATTEMPTS)"
      elif [[ "$label" == *"app"* && "$REQUIRE_BUILD" == "1" ]] && ! has_expected_build "$body"; then
        live=$(grep -Eo 'name="ddl-build" content="[^"]*"' "$body" | head -1)
        echo "  wait [$label] $url -> $code but serves another revision (${live:-no ddl-build meta}) (attempt $attempt/$MAX_ATTEMPTS)"
      else
        echo "  ok   [$label] $url -> $code + semantic body check (attempt $attempt)"
        rm -f "$body"
        return 0
      fi
    else
      echo "  wait [$label] $url -> $code (attempt $attempt/$MAX_ATTEMPTS)"
    fi
    nap=$(( SLEEP_BASE * attempt )); (( nap > 40 )) && nap=40
    sleep "$nap"
  done
  rm -f "$body"
  echo "  DOWN [$label] $url -> last=$code; semantic health not reached after $MAX_ATTEMPTS attempts"
  return 1
}

if [[ $# -eq 0 ]]; then
  echo "usage: $0 '<label>=<url>' ..." >&2
  exit 2
fi

for spec in "$@"; do
  label="${spec%%=*}"
  url="${spec#*=}"
  echo "checking $label ..."
  if ! check_one "$label" "$url"; then
    fail=1
    report+="- **${label}** is DOWN: ${url}"$'\n'
  fi
done

if [[ "$fail" -ne 0 ]]; then
  echo "SMOKE_REPORT<<EOF"        # captured by the workflow to build the issue body
  printf '%s' "$report"
  echo "EOF"
  echo "post-deploy smoke FAILED" >&2
  exit 1
fi
echo "post-deploy smoke PASSED — all surfaces live."
