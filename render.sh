#!/usr/bin/env bash
# Run the full pipeline and render the report to docs/index.html (served by GitHub Pages).
set -euo pipefail
cd "$(dirname "$0")"
QUARTO="${QUARTO:-$(command -v quarto || echo "$HOME/.local/quarto/bin/quarto")}"
for s in script/0*.R; do
  [[ "$s" == "script/00_helpers.R" ]] && continue
  echo "== $s"; Rscript "$s" > /dev/null
done
"$QUARTO" render report/hs_outcomes_report.qmd
mv report/hs_outcomes_report.html docs/index.html
touch docs/.nojekyll
