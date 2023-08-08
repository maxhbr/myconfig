{ config, lib, pkgs, ... }:
let
  cfg = config.myconfig;
in {
  config = (lib.mkIf cfg.dev.embedded.enable {
    home-manager.sharedModules = [{
      programs.fish = {
        shellAbbrs = lib.mapAttrs'
          (name: value: lib.nameValuePair ("west-" + name) value)
          (lib.genAttrs [
            "adafruit_feather_nrf52840"
            "adafruit_feather_stm32f405"
            "b_l4s5i_iot01a"
            "bbc_microbit_v2"
            # "frdm_k64f"
            "lpcxpresso55s69_cpu0"
            # "mimxrt1060_evk"
            "nrf52840dongle_nrf52840"
            "nrf5340dk_nrf5340_cpuapp"
            "nucleo_f767zi"
            "nucleo_h745zi_q_m4"
            "nucleo_h745zi_q_m7"
            "nucleo_l432kc"
            "nucleo_wb55rg"
            "teensy40"
            "teensy41"
            "xiao_ble"
          ] (board: "west-arm build -p always -b ${board} ."));
      };
    }];
  };
};
