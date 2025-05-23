{ pkgs, config, lib, myconfig, inputs, ... }: {
  imports = [
    inputs.impermanence.nixosModule
  ];
  config = {
    environment.persistence."/persistent" = {
      enable = true;
      hideMounts = true;
      directories = [
        "/var/lib/bluetooth"
        "/var/lib/nixos"
        "/var/lib/systemd/coredump"
        "/etc/NetworkManager/system-connections"
        "/etc/ssh/authorized_keys"
      ];
      files = [
        "/etc/machine-id"
        { file = "/var/keys/secret_file"; parentDirectory = { mode = "u=rwx,g=,o="; }; }
        "/etc/ssh/ssh_host_ed25519_key"
        "/etc/ssh/ssh_host_ed25519_key.pub"
        "/etc/ssh/ssh_host_rsa_key"
        "/etc/ssh/ssh_host_rsa_key.pub"
      ];
    };
    system.activationScripts = {
      script.text = ''
        install -d -m 755 "/persistent/home/mhuber" -o ${toString config.users.extraUsers.mhuber.uid} -g ${toString config.users.extraGroups.mhuber.gid}
      '';
    };
    programs.fuse.userAllowOther = true;
    home-manager.users.mhuber = {...}: {
      imports = [
        inputs.impermanence.homeManagerModules.impermanence
      ];
      home.persistence."/persistent/home/mhuber" = {
        directories = [
          "Downloads"
          # "Music"
          # "Documents"
          "MINE"
        #   "VirtualBox VMs"
        #   ".gnupg"
        #   ".ssh"
        #   ".nixops"
        #   ".local/share/keyrings"
        #   ".local/share/direnv"
        #   {
        #     directory = ".local/share/Steam";
        #     method = "symlink";
        #   }
        ];
        # files = [
        #   ".screenrc"
        # ];
        allowOther = true;
      };
    };
  };
}