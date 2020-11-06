{ pkgs, lib, config, ... }: {
  config = {
    environment.systemPackages = with pkgs; [
      (st.override { conf = builtins.readFile ./st-config.h; })
    ];
  };
}
