{ pkgs, ...}:
# let
#   updateDeCONZFirmware = with pkgs; writeScriptBin "updateDeCONZFirmware" ''
#     # Updating Conbee/RaspBee Firmware

#     # ware updates from the web UI will fail silently. Instead, an interactive
#     # utility script is provided as part of this Docker image that you can use
#     # to flash your device's firmware. The script has been tested and verified
#     # to work for Conbee on amd64 Debian linux and armhf Raspbian Stretch and
#     # RaspBee on armhf Raspbian Stretch. To use it, follow the below
#     # instructions:

#     # Check your deCONZ container logs for the update firmware file name: type
#     # docker logs [container name], and look for lines near the beginning of the
#     # log that look like this, noting the .CGF file name listed (you'll need
#     # this later):

#     # GW update firmware found: /usr/share/deCONZ/firmware/deCONZ_Rpi_0x261e0500.bin.GCF
#     # GW firmware version: 0x261c0500
#     # GW firmware version shall be updated to: 0x261e0500

#     # docker stop [container name] or docker-compose down to stop your running
#     # deCONZ container (you must do this or the firmware update will fail).

#     # Invoke the firmware update script:
#     sudo docker run -it --rm \
#          --entrypoint "/firmware-update.sh" \
#          --privileged \
#          --cap-add=ALL \
#          -v /dev:/dev \
#          -v /lib/modules:/lib/modules \
#          -v /sys:/sys \
#          marthoc/deconz:latest

#     # Follow the prompts:

#     # Enter the path (e.g. /dev/ttyUSB0) that corresponds to your device in the listing.
#     # Type or paste the full file name that corresponds to the file name that
#     # you found in the deCONZ container logs in step 1 (or, select a different
#     # filename, but you should have a good reason for doing this).
#     # If the device/path and file name look OK, type Y to start flashing!

#     # Restart your deCONZ container (docker start [container name] or docker-compose up).
#   '';
# in
{ config =
    { users =
        { extraUsers."deconz" =
            { isNormalUser = false;
              group = "deconz";
              uid = 1201;
              home = "/opt/deconz";
              createHome = true;
              shell = pkgs.shadow;
              extraGroups = ["dialout"];
            };
            extraGroups."deconz".gid = 1201;
        };

      networking.firewall.allowedTCPPorts = [ 9443 ];
      networking.firewall.allowedUDPPorts = [ 9443 ];

      docker-containers =
        { deconz =
            { image = "marthoc/deconz:latest";
              # args = ;
              # entrypoint = ;
              volumes =
                [ "/opt/deconz/conf:/root/.local/share/dresden-elektronik/deCONZ"
                  "/etc/localtime:/etc/localtime:ro"
                  # "/etc/timezone:/etc/timezone:ro"
                  "/etc/passwd:/etc/passwd:ro"
                  "/etc/group:/etc/group:ro"
                ];
              extraDockerOptions =
                [ "--net=host"
                  "-e" "DECONZ_WEB_PORT=9080"
                  "-e" "DECONZ_WS_PORT=9443"
                  "--user" "1201:1201"
                  "--device=/dev/ttyACM0"
                  "-e" "DECONZ_DEVICE=/dev/ttyACM0"
                ];
            };
        };

    };
}
