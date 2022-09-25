{ config, lib, pkgs, ... }:

{
  config = { services.fwupd.enable = true; };
}
