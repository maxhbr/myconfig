################################################################################
interpreter=`cat $NIX_CC/nix-support/dynamic-linker`
srcdir=opt/SEGGER/JLink_V${version}

################################################################################
mkdir -p $out/bin $out/lib

################################################################################
for f in `find $srcdir -type f -name 'J*' -executable`; do
  echo "patching $out/bin/"`basename $f`
  install -m 0755 $f $out/bin
  patchelf --interpreter $interpreter $out/bin/`basename $f`
  patchelf --set-rpath $RPATH:$out/lib $out/bin/`basename $f`
done

################################################################################
# Create symlinks for the libraries.
for f in `find $srcdir -type f -name 'lib*' -executable`; do
  basename=`basename $f`
  link=`echo $basename | sed 's/\..*$//g'`

  echo "patching $out/lib/"`basename $f`
  install -m 0755 $f $out/lib
  patchelf --set-rpath $RPATH:$out/lib $out/lib/$basename

  # This is upsetting.
  (cd $out/bin && ln -s ../lib/$basename $link.so)
done
