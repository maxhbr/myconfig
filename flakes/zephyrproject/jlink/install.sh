################################################################################
interpreter="$(cat $NIX_CC/nix-support/dynamic-linker)"
srcdir="opt/SEGGER/JLink_V${version}"

################################################################################
mkdir -p "$out/bin" "$out/lib"
RPATH="$RPATH:$out/lib"

# echo 'installing lib*'
find "$srcdir" -type f -name 'J*' -executable -maxdepth 1 \
  -print \
  -exec install -m 0755 {} "$out/bin" \;
find "$srcdir" -type l -name 'J*' -maxdepth 1 -exec cp -P {} "$out/bin" \;
find "$srcdir" -type f -name 'lib*' -maxdepth 1 \
  -print \
  -exec install -m 0755 {} "$out/lib" \;
find "$srcdir" -type l -name 'lib*' -maxdepth 1 -exec cp -P {} "$out/lib" \;
cp -r opt "$out"

find "$out/lib" -type f -name 'lib*' -print0 |
  while IFS= read -r -d '' f; do
  echo "patching $f"
  basename="$(basename "$f")"
  patchelf --set-rpath "$RPATH" "$f"
  done

find "$out/lib" -name 'lib*' -print0 |
  while IFS= read -r -d '' f; do
    cd $out/bin
    ln -s "$f"
  done

find "$out/bin" -type f -name 'J*' -executable -print0 |
    while IFS= read -r -d '' f; do
  echo "patching $f"
  patchelf --interpreter "$interpreter" "$f"
  patchelf --set-rpath "$RPATH" "$f"
done
