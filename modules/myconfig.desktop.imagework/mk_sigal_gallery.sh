#!/usr/bin/env bash

set -euo pipefail

usage() {
  cat <<EOF
Usage: 
  ${0##*/} [-f] [-o OUTPUT_DIR] [-t TITLE] [-T THEME] [-s SIZE] [-p PASSWORD] SOURCE_DIR
  ${0##*/} -c CONFIG_FILE

Generate a static HTML gallery from SOURCE_DIR using sigal inside a
dedicated virtual-environment located at \${OUTPUT_DIR}.venv.

Options:
  -o OUTPUT_DIR   Destination directory for the gallery (default: ./gallery)
  -t TITLE        Title of the gallery (default: "Gallery")
  -T THEME        Sigal theme to use            (default: galleria)
  -s SIZE         Size of resized image (default: 3840)
  -p PASSWORD     Password for the gallery
  -c CONFIG_File  Rebuild the gallery from config file
  -f              Force overwrite the config file
  -h              Show this help and exit
EOF
  exit 1
}

##############################################################################
# Parse options
##############################################################################
OUT_DIR=""
TITLE="Gallery"
THEME="galleria"
SIZE=3840
PASSWORD=""
REBUILD=false
FORCE=false

while getopts ":o:t:T:s:p:fch" opt; do
  case "$opt" in
    o) OUT_DIR=$(readlink -f "$OPTARG") ;;
    t) TITLE=$OPTARG ;;
    T) THEME=$OPTARG ;;
    s) SIZE=$OPTARG ;;
    p) PASSWORD=$OPTARG ;;
    c) REBUILD=true ;;
    f) FORCE=true ;;
    h) usage ;;
    *) usage ;;
  esac
done
shift $((OPTIND-1))
[[ $# -eq 1 ]] || usage
POSITONAL_ARG="$1"

##############################################################################
# Functions
##############################################################################
create_config() {
  local CONFIG="$1"

  if [[ "$FORCE" != true && -f "$CONFIG" ]]; then
    echo "Config file '$CONFIG' already exists. Use --force to overwrite or '--rebuild $CONFIG' to rebuild."
    exit 1
  fi

  cat > "$CONFIG" <<'EOF'
# --- Auto-generated Sigal configuration ---
source = "@@SOURCE@@"
destination = "@@DEST@@"
# - colorbox (default), galleria, photoswipe, or the path to a custom theme
# directory
theme = "@@THEME@@"

img_size = (@@SIZE@@, @@SIZE@@)

img_extensions = ['.JPG', '.jpg', '.jpeg', '.png', '.gif']

# Uncomment and tweak as desired:
title      = "@@TITLE@@"
# thumb_size = (300, 200)
# use_orig   = False
# plugins    = ["optimize_images"]
EOF

  # Inject variables safely
  sed -i \
    -e 's^@@SOURCE@@^'"$SRC_DIR"'^' \
    -e 's^@@DEST@@^'"$OUT_DIR"'^' \
    -e 's^@@THEME@@^'"$THEME"'^' \
    -e 's^@@SIZE@@^'"$SIZE"'^g' \
    -e 's^@@TITLE@@^'"$TITLE"'^' \
    "$CONFIG"

  if [[ -n "$PASSWORD" ]]; then
    echo "Password: $PASSWORD"
    cat >> "$CONFIG" <<EOF
plugins = ['sigal.plugins.encrypt']
encrypt_options = {
    'password': '@@PASSWORD@@',
    'ask_password': True,
    # 'gcm_tag': 'randomly_generated',
    # 'kdf_salt': 'randomly_generated',
    # 'kdf_iters': 10000,
}
EOF
    sed -i \
      -e 's^@@PASSWORD@@^'"$PASSWORD"'^' \
      "$CONFIG"
  fi
}

build() {
  [[ $# -eq 1 ]] || usage
  local CONFIG="$1"
  if [[ ! -f "$CONFIG" ]]; then
    echo "Config file '$CONFIG' not found."
    exit 1
  fi
  sigal build -c "$CONFIG"
}

##############################################################################
# Main
##############################################################################
if [[ "$REBUILD" == true ]]; then
  rebuild "$POSITONAL_ARG"
else
  SRC_DIR=$(readlink -f "$POSITONAL_ARG")
  [[ -d "$SRC_DIR" ]] || { echo "Source directory '$SRC_DIR' not found."; exit 1; }

  if [[ -z "$OUT_DIR" ]]; then
    OUT_DIR="${SRC_DIR}.gallery"
  fi

  mkdir -p "$OUT_DIR"

  CONFIG="${OUT_DIR}.sigal.conf.py"
  create_config "$CONFIG"
  build "$CONFIG"
fi