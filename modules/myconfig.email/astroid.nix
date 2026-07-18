{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.myconfig;
in
{
  config = lib.mkIf (cfg.email.enable && (builtins.elem "astroid" cfg.email.clients)) {
    home-manager.sharedModules = [
      (
        {
          config,
          lib,
          pkgs,
          myconfig,
          ...
        }:
        {
          config = lib.mkIf (config.home.username == myconfig.user) {
            programs.astroid = {
              enable = true;
            };
            myconfig.homeManagerEmailConfig = [
              {
                astroid = {
                  enable = true;
                }
                # // (lib.mkIf config.programs.msmtp.enable {
                #   sendMailCommand = "${pkgs.msmtp}/bin/msmtp -a ${name}";
                # })
                ;
              }
            ];
          };
        }
      )
    ];
  };
}
