self: super: {
  # thrift93 = self.callPackage ../pkgs/thrift93 {};
  thrift93 = super.modemmanager.overrideDerivation (super: rec {
    name = "thrift-${version}";
    version = "0.9.3";

    src = super.fetchurl {
      url = "http://archive.apache.org/dist/thrift/${version}/${name}.tar.gz";
      sha256 = "17lnchan9q3qdg222rgjjai6819j9k755s239phdv6n0183hlx5h";
    };
  });
}
