{ stdenv, fetchurl, unzip, otfcc, nodejs-6_x, ttfautohint }:

stdenv.mkDerivation rec {
  name = "iosevka-${version}";
  version = "1.12.5";

  set = "custom";
  variants = [
    "light"
    "lightitalic"
    "regular"
    "italic"
    "bold"
    "bolditalic"
  ];
  styleGeneral = [ "expanded" ];
  styleUpright = [
    "v-l-italic"
    "v-i-italic"
    "v-brace-straight"
    "v-m-shortleg"
    "v-zero-dotted"
    "v-asterisk-low"
    "v-caret-low"
    "v-dollar-open"
  ];
  styleItalic = [
    "v-brace-straight"
    "v-m-shortleg"
    "v-zero-dotted"
    "v-asterisk-low"
    "v-caret-low"
    "v-dollar-open"
  ];

  src = fetchurl {
    url = "https://github.com/be5invis/Iosevka/archive/v${version}.tar.gz";
    sha256 = "1y5z4m62ss2819cabi8izdyh7d1rwlliyvr3vf05imxn129vd6pv";
  };

  nativeBuildInputs = [ unzip otfcc nodejs-6_x ttfautohint ];

  patches = [ ./ligatures.patch ];

  buildPhase = ''
    # Fake HOME directory for npm
    export HOME=$TMPDIR
    npm install
    make custom-config \
        set=${set} \
        design='${stdenv.lib.concatStringsSep " " styleGeneral}' \
        upright='${stdenv.lib.concatStringsSep " " styleUpright}' \
        italic='${stdenv.lib.concatStringsSep " " styleItalic}'
    for variant in ${stdenv.lib.concatStringsSep " " variants}; do
      make -f utility/custom.mk dist/iosevka-${set}/iosevka-${set}-$variant.ttf set=${set} __IOSEVKA_CUSTOM_BUILD__=true
    done
  '';

  installPhase = ''
    fontdir=$out/share/fonts/truetype
    mkdir -p $fontdir
    cp -v dist/iosevka-${set}/* $fontdir
  '';

  meta = with stdenv.lib; {
    homepage = "http://be5invis.github.io/Iosevka/";
    downloadPage = "https://github.com/be5invis/Iosevka/releases";
    description = ''
      Slender monospace sans-serif and slab-serif typeface inspired by Pragmata
      Pro, M+ and PF DIN Mono, designed to be the ideal font for programming.
    '';
    license = licenses.ofl;
    platforms = platforms.all;
    maintainers = [ maintainers.fmthoma ];
  };
}
