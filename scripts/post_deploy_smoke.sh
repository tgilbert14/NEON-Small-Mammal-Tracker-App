#!/usr/bin/env bash
# post_deploy_smoke.sh — post-deploy liveness check for the refresh loop.
#
# After the monthly refresh pushes to main, Connect Cloud republishes the app.
# This closes the loop: it confirms the LIVE surfaces actually come back up. It is
# cold-start aware — a Connect Cloud worker can take a minute-plus to wake, so each
# URL is polled with backoff for up to ~5 minutes before it is declared down.
#
# Usage:  post_deploy_smoke.sh "<label>=<url>" ["<label>=<url>" ...]
# Exit 0 if every URL returns a healthy status within the retry budget; 1 otherwise.
# The Connect app must also return the app-specific UI marker and must not match a
# known host error page. This is semantic startup evidence, not an HTTP-only probe.
# On failure the refresh workflow opens/updates a GitHub issue (see refresh-data.yml).

set -uo pipefail

MAX_ATTEMPTS="${SMOKE_MAX_ATTEMPTS:-10}"   # ~10 tries
SLEEP_BASE="${SMOKE_SLEEP_BASE:-5}"        # 5s,10s,15s... capped — ~5 min total
CONNECT_TIMEOUT="${SMOKE_CONNECT_TIMEOUT:-15}"
MAX_TIME="${SMOKE_MAX_TIME:-45}"
APP_MARKER="${SMOKE_APP_MARKER:-ddl-app-ready}"

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
