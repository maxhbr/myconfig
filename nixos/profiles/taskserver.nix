{ config, pkgs, ... }:

{
  services.taskserver = {
    enable = true;
    fqdn = "server";
    listenHost = "::";
    organisations.my-company.users = [ "mhuber" ];
  };
  # networking.firewall.allowedTCPPorts = [ ];
}
