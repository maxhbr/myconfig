#!/usr/bin/env bash
#
# gallery.sh – Build a static HTML photo gallery with Sigal in an isolated venv
#
# Usage:
#   ./gallery.sh [-o OUTPUT_DIR] [-t THEME] SOURCE_DIR
#
# Example:
#   ./gallery.sh -o ~/public_html/trip_gallery ~/Pictures/Spain2025
#
# The script creates or re-uses a virtual-env at “${OUT_DIR}.venv”.
# ------------------------------------------------------------------------------

set -euo pipefail

##############################################################################
# Helper
##############################################################################
usage() {
  cat <<EOF
Usage: ${0##*/} [-o OUTPUT_DIR] [-t THEME] SOURCE_DIR

Generate a static HTML gallery from SOURCE_DIR using sigal inside a
dedicated virtual-environment located at \${OUTPUT_DIR}.venv.

Options:
  -o OUTPUT_DIR   Destination directory for the gallery (default: ./gallery)
  -t THEME        Sigal theme to use            (default: colorbox)
  -h              Show this help and exit
EOF
  exit 1
}

##############################################################################
# Parse options
##############################################################################
OUT_DIR=""
THEME="colorbox"

while getopts ":o:t:h" opt; do
  case "$opt" in
    o) OUT_DIR=$(readlink -f "$OPTARG") ;;
    t) THEME=$OPTARG ;;
    h) usage ;;
    *) usage ;;
  esac
done
shift $((OPTIND-1))

[[ $# -eq 1 ]] || usage
SRC_DIR=$(readlink -f "$1")
[[ -d "$SRC_DIR" ]] || { echo "❌ Source directory '$SRC_DIR' not found."; exit 1; }


# if OUT_DIR is not set, replace it with $SRC_DIR.gallery
if [[ -z "$OUT_DIR" ]]; then
  OUT_DIR="${SRC_DIR}.gallery"
fi

# Create output directory
mkdir -p "$OUT_DIR"

##############################################################################
# Prepare virtual environment
##############################################################################
VENV_DIR="${HOME}/.$(basename "$0").venv"

if [[ ! -d "$VENV_DIR" ]]; then
  echo "🛠  Creating virtual-env at '$VENV_DIR' ..."
  python3 -m venv "$VENV_DIR"
fi

# shellcheck disable=SC1090
source "$VENV_DIR/bin/activate"

# Install (or upgrade) sigal & Pillow only if missing
if ! python -m pip show sigal > /dev/null 2>&1; then
  echo "📦 Installing sigal ..."
  python -m pip install --upgrade --quiet sigal pillow
fi

##############################################################################
# Build a temporary Sigal config
##############################################################################
CONFIG="${OUT_DIR}.sigal.conf.py"

cat > "$CONFIG" <<'EOF'
# --- Auto-generated Sigal configuration ---
source = "$SOURCE"
destination = "$DEST"
theme = "$THEME"

# - colorbox (default), galleria, photoswipe, or the path to a custom theme
# directory
theme = "galleria"

# Size of resized image (default: (640, 480))
img_size = (3840, 3840)

# File extensions that should be treated as images
img_extensions = ['.JPG', '.jpg', '.jpeg', '.png', '.gif']

# Uncomment and tweak as desired:
# title      = "My Photo Gallery"
# thumb_size = (300, 200)
# use_orig   = False
# plugins    = ["optimize_images"]
EOF

# Inject variables safely
sed -i \
  -e "s^\$SOURCE^$SRC_DIR^" \
  -e "s^\$DEST^$OUT_DIR^" \
  -e "s^\$THEME^$THEME^" \
  "$CONFIG"

##############################################################################
# Run Sigal
##############################################################################
echo "🚀 Building gallery with Sigal (theme: $THEME)…"
python -m sigal build -c "$CONFIG" --quiet
echo "✅ Gallery ready: '$OUT_DIR/index.html'"

##############################################################################
# End
##############################################################################
