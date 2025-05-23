{ pkgs, config, lib, myconfig, inputs, ... }: {
  imports = [
    inputs.impermanence.nixosModule
  ];
  config = {
    environment.persistence."/persistent/priv" = {
      enable = true;
      hideMounts = true;
      directories = [
        "/var/lib/bluetooth"
        "/var/lib/nixos"
        "/etc/NetworkManager/system-connections"
        "/etc/ssh/authorized_keys"
      ];
      files = [
        # "/etc/machine-id"
        { file = "/var/keys/secret_file"; parentDirectory = { mode = "u=rwx,g=,o="; }; }
        "/etc/ssh/ssh_host_ed25519_key"
        "/etc/ssh/ssh_host_ed25519_key.pub"
        "/etc/ssh/ssh_host_rsa_key"
        "/etc/ssh/ssh_host_rsa_key.pub"
      ];
    };
    environment.persistence."/persistent/cache" = {
      enable = true;
      hideMounts = true;
      directories = [
        "/var/lib/ollama"
        "/var/lib/systemd/coredump"
      ];
    };
    system.activationScripts = {
      script.text = ''
        install -d -m 755 "/persistent/priv/home/mhuber" -o ${toString config.users.extraUsers.mhuber.uid} -g ${toString config.users.extraGroups.mhuber.gid}
        install -d -m 755 "/persistent/work/home/mhuber" -o ${toString config.users.extraUsers.mhuber.uid} -g ${toString config.users.extraGroups.mhuber.gid}
      '';
    };
    programs.fuse.userAllowOther = true;
    home-manager.users.mhuber = {...}: {
      imports = [
        inputs.impermanence.homeManagerModules.impermanence
      ];
      home.persistence."/persistent/priv/home/mhuber" = {
        directories = [
          "myconfig"
          "Downloads"
          "Documents"
          "MINE"
          "_screenshots"
          ".gnupg"
        #   ".ssh"
        #   ".nixops"
        #   ".local/share/keyrings"
        #   ".local/share/direnv"
        #   {
        #     directory = ".local/share/Steam";
        #     method = "symlink";
        #   }
          "Maildir/alfa"
          "Maildir/gmail"
          "Maildir/mail"
        ];
        # files = [
        #   ".screenrc"
        # ];
        allowOther = true;
      };
      home.persistence."/persistent/work/home/mhuber" = {
        directories = [
          "TNG"
          "Maildir/tng"
        ];
      };
    };
  };
}