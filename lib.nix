let
  secretsDir = ./secrets;
  importall = path:
    if builtins.pathExists path then
      let content = builtins.readDir path;
      in map (n: import (path + ("/" + n))) (builtins.filter (n:
        builtins.match ".*\\.nix" n != null
        || builtins.pathExists (path + ("/" + n + "/default.nix")))
        (builtins.attrNames content))
    else
      [ ];
  lib = import <nixpkgs/lib>;
in rec {
  getSecretPath = hostName: fileName: (secretsDir + "/${hostName}/${fileName}");
  getSecret = hostName: fileName:
    builtins.readFile (getSecretPath hostName fileName);

  getSecretNoNewline = hostName: fileName:
    lib.removeSuffix "\n" (getSecret hostName fileName);

  makeOptionalBySecrets = conf:
    lib.mkIf (builtins.pathExists (secretsDir + "/README.md")) conf;

  makeOptionalListBySecrets = list:
    if (builtins.pathExists (secretsDir + "/README.md")) then list else [ ];

  mkFromBin = namePrefix: binDir: pkgs:
    let
      binDerivation = pkgs.stdenv.mkDerivation rec {
        version = "1.0";
        name = "${namePrefix}-bin-${version}";

        src = binDir;

        buildPhase = "";

        installPhase = ''
          bin=$out/bin
          mkdir -p $bin
          cp * $bin
          chmod -R +x $bin
        '';
      };
    in lib.mkIf (builtins.pathExists binDir) {
      home-manager.users.mhuber = { home.packages = [ binDerivation ]; };
    };

  mkFromHostBin = hostName: pkgs:
    let binDir = ./host + ".${hostName}/bin";
    in mkFromBin hostName binDir pkgs;

  mkFromSecretBin = hostName: pkgs:
    let binDir = secretsDir + "/${hostName}/bin";
    in mkFromBin "${hostName}-secrets" binDir pkgs;

  mkHost = hostName: addConfig:
    { config, lib, pkgs, ... }@args: {
      imports = [
        (./host + ".${hostName}")
        (secretsDir + "/${hostName}")
        (addConfig args)
        (mkFromHostBin hostName pkgs)
        (mkFromSecretBin hostName pkgs)
        {
          deployment = let ipFile = (secretsDir + "/${hostName}/ip");
          in lib.mkIf (builtins.pathExists ipFile) {
            targetHost = getSecretNoNewline hostName "ip";
          };
        }
        {
          assertions = [{
            assertion = config.networking.hostName == hostName;
            message = "hostname should be set!";
          }
          # { assertion = secretsConfig.users.users.mhuber.hashedPassword != null;
          #   message = "password should be overwritten in ./nixops-secrets.nix";
          # }
            ];
        }
      ] ++ (importall (secretsDir + "/${hostName}/imports"));
      networking.domain = "maxhbr.de";
    };

  announceHost = hostName:
    let hostIp = getSecretNoNewline hostName "ip";
    in { ... }: {
      config = makeOptionalBySecrets {
        networking.extraHosts = ''
          ${hostIp} ${hostName}
          ${hostIp} ${hostName}.maxhbr.de
        '';
        home-manager.users.mhuber = {
          home.file = {
            ".ssh/imports/my-${hostName}.config".text = ''
              Host ${hostName}
                HostName ${hostIp}
                User mhuber
            '';
          };
        };
      };
    };

  mkHostNixops = hostName: addConfig: {
    network.description = "myconfig-" + hostName;
    "${hostName}" = mkHost hostName addConfig;
  };

  fixIp = hostName: deviceName: {
    networking = {
      interfaces."${deviceName}".ipv4.addresses = [{
        address = let newIpPath = getSecretPath hostName "newIp";
        in if builtins.pathExists newIpPath then
          getSecretNoNewline hostName "newIp"
        else
          getSecretNoNewline hostName "ip";
        prefixLength = 24;
      }];
      defaultGateway = "192.168.1.1";
      nameservers = [ "8.8.8.8" "8.8.4.4" ];
    };
  };

  setupAsWireguardClient = wgip:
    ({ config, lib, pkgs, ... }@args:
      makeOptionalBySecrets {
        environment.systemPackages = [ pkgs.wireguard ];
        networking.wireguard.interfaces = {
          wg0 = {
            ips = [
              (wgip + "/24")
            ]; # Determines the IP address and subnet of the server's end of the tunnel interface.
            privateKeyFile = "/etc/wireguard/wg-private";
            peers = [{
              publicKey = getSecretNoNewline "vserver" "wireguard-keys/public";
              # allowedIPs = [ "0.0.0.0/0" ];
              # Or forward only particular subnets
              allowedIPs = [ "10.199.199.0/24" ];
              endpoint = ((getSecretNoNewline "vserver" "ip") + ":51820");
              persistentKeepalive =
                25; # Send keepalives every 25 seconds. Important to keep NAT tables alive.
            }];
          };
        };
      });

  deployWireguardKeys = hostName: {
    deployment.keys = {
      wg-private = {
        text = getSecret hostName "wireguard-keys/private";
        destDir = "/etc/wireguard";
        user = "root";
        group = "root";
        permissions = "0400";
      };
      wg-public = {
        text = getSecret hostName "wireguard-keys/public";
        destDir = "/etc/wireguard";
        user = "root";
        group = "root";
        permissions = "0444";
      };
    };
  };

  deploySSHUserKeys = hostName: algo: {
    deployment.keys = {
      "id_${algo}" = {
        text = getSecret hostName "ssh/id_${algo}";
        destDir = "/home/mhuber/.ssh";
        user = "mhuber";
        group = "mhuber";
        permissions = "0400";
      };
      "id_${algo}.pub" = {
        text = getSecret hostName "ssh/id_${algo}.pub";
        destDir = "/home/mhuber/.ssh";
        user = "mhuber";
        group = "mhuber";
        permissions = "0444";
      };
    };
  };

  setupAsBackupTarget = home: keys: {
    config = {
      users = {
        extraUsers.backup = {
          isNormalUser = true;
          group = "backup";
          uid = 1100;
          inherit home;
          createHome = false;
          openssh.authorizedKeys = { inherit keys; };
        };
        extraGroups.backup.gid = 1100;
      };
    };
  };

  # https://nixos.wiki/wiki/Binary_Cache
  setupNixServe = keys: {
    nix.sshServe = {
      enable = true;
      inherit keys;
    };
    users.extraUsers.nix-ssh = { openssh.authorizedKeys = { inherit keys; }; };
  };

  setupNasNFS = name: {
    fileSystems."/mnt/nas-${name}" = {
      device = "nas:/${name}";
      fsType = "nfs";
      options = [ "x-systemd.automount" "noauto" ];
    };
  };

  setupAsBuildMachine = authorizedKeys: {
    users.extraUsers.nixBuild = {
      name = "nixBuild";
      useDefaultShell = true;
      openssh.authorizedKeys.keys = authorizedKeys;
    };
    nix = {
      allowedUsers = [ "nixBuild" ];
      trustedUsers = [ "nixBuild" ];
    };
  };

  setupBuildSlave = host: speedFactor: systems: sshKeyText: publicKey:
    let keyName = "nixBuildPrivKey.${host}";
    in {
      deployment.keys = {
        "${keyName}" = {
          text = sshKeyText;
          destDir = "/etc/nix";
          user = "root";
          group = "keys";
          permissions = "0400";
        };
      };
      nix.buildMachines = map (system: {
        hostName = "builder.${host}";
        maxJobs = 6;
        supportedFeatures = [ "nixos-test" "benchmark" "big-parallel" "kvm" ];
        mandatoryFeatures = [ ];
        sshUser = "nixBuild";
        sshKey = "/etc/nix/${keyName}";
        inherit speedFactor system;
      }) systems;
      nix.distributedBuilds = true;
      # optional, useful when the builder has a faster internet connection than yours
      nix.extraOptions = ''
        builders-use-substitutes = true
      '';
      services.openssh.knownHosts = {
        "builder.${host}" = { inherit publicKey; };
      };
      programs.ssh.extraConfig = ''
        Host builder.${host}
            HostName ${host}
            User nixBuild
            IdentitiesOnly yes
            IdentityFile /etc/nix/${keyName}
            StrictHostKeyChecking accept-new
            ConnectTimeout 2
      '';
    };

  mkSyncthingDevice = hostName: introducer:
    let
      baseDevice = import (secretsDir + "/${hostName}/syncthing/device.nix");
      ipFile = (secretsDir + "/${hostName}/ip");
      ipAddress = if (builtins.pathExists ipFile) then
        let ip = getSecretNoNewline hostName "ip"; in [ "tcp://${ip}" ]
      else
        [ ];
    in {
      "${hostName}" = {
        name = hostName;
        inherit (baseDevice) id;
        addresses = ipAddress ++ baseDevice.addresses;
        inherit introducer;
      };
    };

  setupSyncthing = hostName: devices: folders: {
    deployment.keys = {
      "cert.pem" = {
        text = getSecret hostName "syncthing/cert.pem";
        destDir = "/etc/syncthing";
        user = "root";
        group = "root";
        permissions = "0400";
      };
      "key.pem" = {
        text = getSecret hostName "syncthing/key.pem";
        destDir = "/etc/syncthing";
        user = "root";
        group = "root";
        permissions = "0400";
      };
    };
    services.syncthing = {
      enable = true;
      declarative = {
        cert = "/etc/syncthing/cert.pem";
        key = "/etc/syncthing/key.pem";
        inherit devices folders;
      };
    };
  };

  # # generate with:
  # # $ nix-store --generate-binary-cache-key binarycache.example.com cache-priv-key.pem cache-pub-key.pem
  # # see:
  # # - https://nixos.wiki/wiki/Binary_Cache
  # deployBinaryCacheKeys = hostName:
  #   let
  #     keyName = "cache-priv-key.pem";
  #   in
  #   { deployment.keys =
  #       { "${keyName}" =
  #           { text = builtins.readFile (secretsDir + "/${hostName}/binary-cache-keys/${keyName}");
  #             user = "nix-serve";
  #             group = "nix-serve";
  #             permissions = "0400";
  #           };
  #       };
  #     # services.nix-serve =
  #     #   { enable = true;
  #     #     secretKeyFile = "/run/keys/${keyName}";
  #     #   };
  #   };
}
