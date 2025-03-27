{ pkgs, ... }: {
  config = {
    powerManagement.powerDownCommands = ''
      ${pkgs.procps}/bin/pkill -9 sshfs
    '';
  };
}

