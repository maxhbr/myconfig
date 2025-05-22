{ pkgs, config, lib, myconfig, inputs, ... }: {
  imports = [
    inputs.impermanence.nixosModule
  ];
  config = {
    environment.persistence."/persistent" = {
      enabled = true;
      hideMounts = true;
      directories = [
        "/var/lib/bluetooth"
        "/var/lib/nixos"
        "/var/lib/systemd/coredump"
        "/etc/NetworkManager/system-connections"
      ];
      files = [
        "/etc/machine-id"
        { file = "/var/keys/secret_file"; parentDirectory = { mode = "u=rwx,g=,o="; }; }
      ];
    };
    home-manager.users.mhuber = {...}: {
      imports = [
        inputs.impermanence.homeManagerModules.impermanence
      ];
      home.persistence."/persistent/home/mhuber" = {
        directories = [
          "Downloads"
          "Music"
          "Documents"
          "MINE"
          "VirtualBox VMs"
          ".gnupg"
          ".ssh"
          ".nixops"
          ".local/share/keyrings"
          ".local/share/direnv"
          {
            directory = ".local/share/Steam";
            method = "symlink";
          }
        ];
        files = [
          ".screenrc"
        ];
        allowOther = true;
      };
    }
  };
}