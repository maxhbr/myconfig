#!/usr/bin/env bash
# see:
# - https://phoscon.de/en/conbee2/install#docker
# - https://hub.docker.com/r/marthoc/deconz

set -e
DECONZ="${DECONZ:-/home/deconz}"
DECONZ_USER="${DECONZ_USER:-deconz}"

have() { type "$1" &> /dev/null; }
setup() {
    echo "... SETUP"
    if ! id -u "$DECONZ_USER"; then
        if have nixos-rebuild; then
            echo "use nixos to create the user"
            exit 1
        fi
        sudo useradd -d "$DECONZ" -r -s /sbin/nologin "$DECONZ_USER"
        sudo usermod -a -G $DECONZ_USER "$USER"
        sudo usermod -a -G dialout "$DECONZ_USER"
    fi
    sudo mkdir -p "$DECONZ/conf"
    sudo chown -R $DECONZ_USER:$DECONZ_USER "$DECONZ"
}
update() {
    echo "... UPDATE"
    sudo docker stop deconz && sudo docker rm deconz || true
    sudo docker pull marthoc/deconz:latest
}
updateFirmware() {
#     Updating Conbee/RaspBee Firmware

# # Firmware updates from the web UI will fail silently. Instead, an interactive utility script is provided as part of this Docker image that you can use to flash your device's firmware. The script has been tested and verified to work for Conbee on amd64 Debian linux and armhf Raspbian Stretch and RaspBee on armhf Raspbian Stretch. To use it, follow the below instructions:

# #     Check your deCONZ container logs for the update firmware file name: type docker logs [container name], and look for lines near the beginning of the log that look like this, noting the .CGF file name listed (you'll need this later):

# #     GW update firmware found: /usr/share/deCONZ/firmware/deCONZ_Rpi_0x261e0500.bin.GCF
# #     GW firmware version: 0x261c0500
# #     GW firmware version shall be updated to: 0x261e0500

# #     docker stop [container name] or docker-compose down to stop your running deCONZ container (you must do this or the firmware update will fail).

# #     Invoke the firmware update script:
    sudo docker run -it --rm --entrypoint "/firmware-update.sh" --privileged --cap-add=ALL -v /dev:/dev -v /lib/modules:/lib/modules -v /sys:/sys marthoc/deconz

# #     Follow the prompts:

# #     Enter the path (e.g. /dev/ttyUSB0) that corresponds to your device in the listing.
# #     Type or paste the full file name that corresponds to the file name that you found in the deCONZ container logs in step 1 (or, select a different filename, but you should have a good reason for doing this).
# #     If the device/path and file name look OK, type Y to start flashing!

# #     Restart your deCONZ container (docker start [container name] or docker-compose up).
}
run() {
    args=""
    if [[ -f /etc/localtime ]]; then
        args="$args -v /etc/localtime:/etc/localtime:ro"
    fi
    if [[ -f /etc/timezone ]]; then
        args="$args -v /etc/timezone:/etc/timezone:ro"
    fi

    DEVICE="/dev/ttyACM0"
    echo "... RUN"
    sudo docker run -d \
        --name=deconz \
        --net=host \
        --restart=unless-stopped \
        $args \
        -v "$DECONZ/conf":/root/.local/share/dresden-elektronik/deCONZ \
        --device="$DEVICE" \
        -e DECONZ_DEVICE="$DEVICE" \
        -e DECONZ_WEB_PORT=9080 \
        -e DECONZ_WS_PORT=9443 \
        marthoc/deconz
}

setup
update
if [[ "$1" == "update-firmware"]]; then
    updateFirmware
fi
run

