cur: hostName:
let importall = import ./importall.nix;
in [{ config = { networking.hostName = hostName; }; }]
++ (importall (cur + "/imports/"))
