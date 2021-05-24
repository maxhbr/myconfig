{ stdenv, lib, requireFile, autoPatchelfHook, makeDesktopItem, copyDesktopItems
, fontconfig, freetype, libusb, libICE, libSM, udev, libX11, libXext, libXcursor
, libXfixes, libXrender, libXrandr }:

let
  architectures = {
    i686-linux = "i386";
    x86_64-linux = "x86_64";
    armv7l-linux = "arm";
    aarch64-linux = "arm64";
  };

  architecture = architectures.${stdenv.hostPlatform.system};

  hashes = {
    x86_64-linux =
      "9560f026f66b5470f5536877043e6e6268c96eb9d3ee9d7f87542e8e06332e76";
    i686-linux =
      "e2f2a92fc9e31527ba019ee80de8a8808e9c967a73566a46350eb409444a8645";
    armv7l-linux =
      "633b054645605f1d4d964f34b9e9fa9f2f265f7f48e94332cb9bbb65ce76bd45";
    aarch64-linux =
      "9f0728c84a7c134c52c020b56decf41abaef081e33eca49daf367f58e6e050a2";
  };

  hash = hashes.${stdenv.hostPlatform.system};

  desktopItems = [
    (makeDesktopItem {
      name = "j-flash";
      desktopName = "SEGGER - J-Flash";
      categories = "Development;Qt;";
      exec = "JFlashExe";
      comment =
        "An application to program data images to the flash of a target device.";
    })
    (makeDesktopItem {
      name = "j-flash-lite";
      desktopName = "SEGGER - J-Flash Lite";
      categories = "Development;Qt;";
      exec = "JFlashLiteExe";
      comment =
        "Flash programming application to program data images to the flash of a target device (lite version for J-Link BASE and EDU).";
    })
    (makeDesktopItem {
      name = "j-flash-spi";
      desktopName = "SEGGER - J-Flash SPI";
      categories = "Development;Qt;";
      exec = "JFlashSPIExe";
      comment =
        "Flash programming application, which allows direct programming of SPI flashes, without any additional hardware.";
    })
    (makeDesktopItem {
      name = "j-link-config";
      desktopName = "SEGGER - J-Link Configurator";
      categories = "Development;Qt;HardwareSettings;";
      exec = "JLinkConfigExe";
      comment =
        "Allows configuration of USB identification as well as TCP/IP identification of J-Link.";
    })
    (makeDesktopItem {
      name = "j-link-gdb-server";
      desktopName = "SEGGER - J-Link GDB Server";
      categories = "Development;Debugger;Qt;";
      exec = "JLinkGDBServer";
      comment =
        "A remote server for GDB making it possible for GDB to connect to and communicate with the target device via J-Link.";
    })
    (makeDesktopItem {
      name = "j-link-license-manager";
      desktopName = "SEGGER - J-Link License Manager";
      categories = "Development;Qt;";
      exec = "JLinkLicenseManager";
    })
    (makeDesktopItem {
      name = "j-link-rtt-viewer";
      desktopName = "SEGGER - J-Link RTT Viewer";
      categories = "Development;Debugger;Monitor;Qt;";
      exec = "JLinkRTTViewerExe";
    })
    (makeDesktopItem {
      name = "j-link-registration";
      desktopName = "SEGGER - JLink Registration";
      categories = "Development;Qt;";
      exec = "JLinkRegistration";
    })
    (makeDesktopItem {
      name = "j-link-remote-server";
      desktopName = "SEGGER - J-Link Remote Server";
      categories = "Development;Qt;";
      exec = "JLinkRemoteServer";
      comment =
        "Utility which provides the possibility to use J-Link / J-Trace remotely via TCP/IP.";
    })
    (makeDesktopItem {
      name = "j-link-swo-viewer";
      desktopName = "SEGGER - J-Link SWO Viewer";
      categories = "Development;Debugger;Monitor;Qt;";
      exec = "JLinkSWOViewer";
      comment = "Displays the terminal output of the target using the SWO pin.";
    })
    (makeDesktopItem {
      name = "j-mem";
      desktopName = "SEGGER - J-Mem";
      categories = "Development;Debugger;Monitor;Qt;";
      exec = "JMemExe";
      comment =
        "Application to display and modify the RAM and SFRs (Special Function Registers) of target systems while the target is running.";
    })
  ];

in stdenv.mkDerivation rec {
  pname = "j-link";
  version = "V720a";

  src = requireFile {
    name = "JLink_Linux_${version}_${architecture}.tgz";
    url =
      "https://www.segger.com/downloads/jlink#J-LinkSoftwareAndDocumentationPack";
    sha256 = hash;
  };

  dontConfigure = true;
  dontBuild = true;
  dontStrip = true;
  preferLocalBuild = true;

  nativeBuildInputs = [ copyDesktopItems autoPatchelfHook ];

  buildInputs = [
    udev
    stdenv.cc.cc.lib
    fontconfig
    freetype
    libICE
    libSM
    libX11
    libXext
    libXcursor
    libXfixes
    libXrender
    libXrandr
  ];

  runtimeDependencies = [ udev ];

  inherit desktopItems;

  installPhase = ''
    runHook preInstall
    mkdir -p $out/{JLink,bin}
    cp -R * $out/JLink
    ln -s $out/JLink/J* $out/bin/

    # Fails because of https://github.com/NixOS/patchelf/issues/255, waiting for 0.13 or whatever.
    rm -f $out/JLink/JLinkSTM32Exe

    rm -r $out/bin/JLinkDevices.xml
    install -D -t $out/lib/udev/rules.d 99-jlink.rules
    runHook postInstall
  '';

  preFixup = ''
    patchelf --add-needed libudev.so.1 $out/JLink/libjlinkarm.so
  '';

  meta = with lib; {
    homepage = "https://www.segger.com/downloads/jlink";
    description = "SEGGER J-Link";
    license = licenses.unfree;
    platforms = platforms.linux;
    maintainers = with maintainers; [ liff reardencode maxhbr ];
  };
}
