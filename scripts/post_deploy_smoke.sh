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
# On failure the refresh workflow opens/updates a GitHub issue (see refresh-data.yml).

set -uo pipefail

MAX_ATTEMPTS="${SMOKE_MAX_ATTEMPTS:-10}"   # ~10 tries
SLEEP_BASE="${SMOKE_SLEEP_BASE:-5}"        # 5s,10s,15s... capped — ~5 min total
CONNECT_TIMEOUT="${SMOKE_CONNECT_TIMEOUT:-15}"
MAX_TIME="${SMOKE_MAX_TIME:-45}"

fail=0
report=""

check_one() {
  local label="$1" url="$2" attempt code
  for ((attempt = 1; attempt <= MAX_ATTEMPTS; attempt++)); do
    # Follow redirects; a 2xx or 3xx final code counts as up. Connect Cloud can
    # answer 200 while still waking, which is fine — we only care it responds.
    code=$(curl -sS -o /dev/null -w '%{http_code}' -L \
                --connect-timeout "$CONNECT_TIMEOUT" --max-time "$MAX_TIME" \
                -A 'ddl-uptime-smoke/1.0' "$url" 2>/dev/null || echo "000")
    if [[ "$code" =~ ^(2|3)[0-9][0-9]$ ]]; then
      echo "  ok   [$label] $url -> $code (attempt $attempt)"
      return 0
    fi
    echo "  wait [$label] $url -> $code (attempt $attempt/$MAX_ATTEMPTS)"
    local nap=$(( SLEEP_BASE * attempt )); (( nap > 40 )) && nap=40
    sleep "$nap"
  done
  echo "  DOWN [$label] $url -> last=$code after $MAX_ATTEMPTS attempts"
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
