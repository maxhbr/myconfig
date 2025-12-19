{ pkgs, ... }:

let
  disableInputWakeScript = pkgs.writeShellApplication {
    name = "disable-input-wake";
    runtimeInputs = [ pkgs.coreutils ];
    text = ''
      ### Disable wake from internal keyboard (serio / i8042)
      if [ -e /sys/devices/platform/i8042/serio0/power/wakeup ]; then
        echo "Disabling keyboard wake..."
        echo disabled > /sys/devices/platform/i8042/serio0/power/wakeup || true
      fi

      ### Disable wake from internal touchpad (I2C-HID through AMDI0010)
      ### Adapted to your detected device:
      ### /sys/devices/platform/AMDI0010:03/i2c-2/i2c-PIXA3854:00/power/wakeup
      if [ -e /sys/devices/platform/AMDI0010:03/i2c-2/i2c-PIXA3854:00/power/wakeup ]; then
        echo "Disabling touchpad wake..."
        echo disabled > /sys/devices/platform/AMDI0010:03/i2c-2/i2c-PIXA3854:00/power/wakeup || true
      fi
    '';
  };
in
{

  systemd.services.disable-input-wake = {
    description = "Disable wake from internal keyboard and touchpad";
    wantedBy = [ "multi-user.target" ];
    after = [ "sysinit.target" ];

    serviceConfig = {
      Type = "oneshot";
      ExecStart = "${disableInputWakeScript}/bin/disable-input-wake";
    };
  };

  services.udev.extraRules = ''
    # Do not let the lid ACPI button wake the machine (if you want that too):
    # ACTION=="add", SUBSYSTEM=="acpi", DRIVERS=="button", ATTRS{hid}=="PNP0C0D", ATTR{power/wakeup}="disabled"

    # Disable wakeup from the built-in keyboard (atkbd on serio/i8042)
    ACTION=="add", SUBSYSTEM=="serio", DRIVERS=="atkbd", ATTR{power/wakeup}="disabled"

    # Disable wakeup from the touchpad (any input device whose name contains "Touchpad")
    ACTION=="add", SUBSYSTEM=="input", ATTR{name}=="*Touchpad*", ATTR{power/wakeup}="disabled"

    # Disable wakeup from the touchpad (any input device whose name contains "Touchpad")
    ACTION=="add", SUBSYSTEM=="input", ATTR{name}=="*Mouse*", ATTR{power/wakeup}="disabled"
  '';
}
