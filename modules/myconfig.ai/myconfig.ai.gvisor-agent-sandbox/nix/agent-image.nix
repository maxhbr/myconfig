{
  lib,
  dockerTools,
  buildEnv,
  bashInteractive,
  cacert,
  coreutils-full,
  curl,
  diffutils,
  fd,
  findutils,
  gawk,
  gcc,
  git,
  gnugrep,
  gnumake,
  gnupatch,
  gnused,
  gnutar,
  gzip,
  jq,
  less,
  nodejs,
  openssh,
  pkg-config,
  procps,
  python3,
  ripgrep,
  shadow,
  util-linux,
  which,

  imageName ? "localhost/agent-dev",
  imageTag ? "latest",
  # Toolchain visible inside the sandbox. Override to slim down or extend.
  packages ? null,
  # Convenience: add packages (for example a coding-agent CLI) without
  # restating the whole default list.
  extraPackages ? [ ],
}:

let
  defaultPackages = [
    bashInteractive
    cacert
    coreutils-full
    curl
    diffutils
    fd
    findutils
    gawk
    gcc
    git
    gnugrep
    gnumake
    gnupatch
    gnused
    gnutar
    gzip
    jq
    less
    nodejs # provides node and npm
    openssh # ssh client for git remotes
    pkg-config
    procps
    python3
    ripgrep
    shadow # getent, id helpers
    util-linux
    which
  ];

  rootPackages = (if packages == null then defaultPackages else packages) ++ extraPackages;

  imageRoot = buildEnv {
    name = "agent-sandbox-root";
    paths = rootPackages;
    pathsToLink = [
      "/bin"
      "/lib"
      "/libexec"
      "/share"
      "/etc"
      "/include"
    ];
    ignoreCollisions = true;
  };
in
dockerTools.buildLayeredImage {
  name = imageName;
  tag = imageTag;

  contents = [
    imageRoot
    dockerTools.usrBinEnv # /usr/bin/env
    dockerTools.binSh # /bin/sh
    dockerTools.caCertificates # /etc/ssl/certs/ca-bundle.crt
    dockerTools.fakeNss # minimal /etc/passwd, /etc/group, /etc/nsswitch.conf
  ];

  # Runs in the customisation layer root, so paths are relative.
  extraCommands = ''
    mkdir -p workspace tmp
    mkdir -p home/agent/.cache home/agent/.config home/agent/.local/state
    chmod 1777 tmp
    chmod -R 0777 home/agent
  '';

  config = {
    Cmd = [ "/bin/bash" ];
    WorkingDir = "/workspace";
    Env = [
      "PATH=/bin:/usr/bin"
      "HOME=/home/agent"
      "XDG_CONFIG_HOME=/home/agent/.config"
      "XDG_CACHE_HOME=/home/agent/.cache"
      "XDG_STATE_HOME=/home/agent/.local/state"
      "SSL_CERT_FILE=/etc/ssl/certs/ca-bundle.crt"
      "GIT_SSL_CAINFO=/etc/ssl/certs/ca-bundle.crt"
      "NIX_SSL_CERT_FILE=/etc/ssl/certs/ca-bundle.crt"
      "LANG=C.UTF-8"
      "TERM=xterm-256color"
      "PAGER=less"
    ];
    Labels = {
      "org.opencontainers.image.title" = "gvisor-agent-sandbox";
      "org.opencontainers.image.description" = "Generic coding-agent sandbox image, built with Nix";
    };
  };

  # `imageName` and `imageTag` are exposed by dockerTools itself, so the
  # session manager can derive the default image reference from this package.
}
