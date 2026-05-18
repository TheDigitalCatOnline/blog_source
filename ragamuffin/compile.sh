#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

sass --no-source-map --style=expanded "$SCRIPT_DIR/static/scss/main.scss" "$SCRIPT_DIR/static/css/main.css"

echo "Compiled static/scss/main.scss → static/css/main.css"
