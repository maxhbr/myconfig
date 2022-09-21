{ config, lib, pkgs, ... }:

{
  imports = [./neomutt];
  config = {
    accounts.email = {
      accounts = {
        mail = {
          primary = true;
          flavor = "plain";
        };
      };
    };
  };
}
