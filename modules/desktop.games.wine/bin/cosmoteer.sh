#!/usr/bin/env bash

set -ex

root=$(readlink -f "$HOME/Desktop/Cosmoteer/")
export prereq_dir="$root/prereq"
export WINEARCH=win32
export WINEDEBUG=fixme-all
if [[ "$WINEARCH" == "win64" ]]; then
    export WINEPREFIX="$root/WINEPREFIX"
    export WINE="wine64"
else
    export WINEPREFIX="$root/WINEPREFIX_32"
    export WINE="wine"
fi

function error_msg() {
	  >&2 echo -en "\e[1;31mERROR:\e[0m"
	  >&2 echo " $2"
	  exit $1
}

function header_msg() {
	  >&2 echo
	  >&2 echo -en "\e[1m"
	  >&2 echo -n "► $1..."
	  >&2 echo -e "\e[0m"
}

function info_msg() {
	  >&2 echo -en "\e[1;32mINFO:\e[0m"
	  >&2 echo " $1"
}

function download_prerequisites() {
	header_msg "Downloading prerequisites"

	wget https://download.microsoft.com/download/D/D/3/DD35CC25-6E9C-484B-A746-C5BE0C923290/NDP47-KB3186497-x86-x64-AllOS-ENU.exe -cP "$prereq_dir"

	wget https://cosmoteer.net/version.txt -cP "$prereq_dir"

	ver="$(head -n1 "$prereq_dir/version.txt")"
	cosmoteer_url="$(tail -n2 "$prereq_dir/version.txt" | head -n1 | sed "s#{0}#$ver#")"
  echo "ver=$ver"
  echo "cosmoteer_url=$cosmoteer_url"
	wget "$cosmoteer_url" -cO "$prereq_dir/Cosmoteer_Setup.exe"
	info_msg "Cosmoteer Setup is '$prereq_dir/Cosmoteer_Setup.exe'."
}

function prepare_wineprefix() {

	header_msg "Initializing WINE prefix"
	wineboot --update
	wineserver --wait
	info_msg "Wine prefix '$WINEPREFIX' initialized."

	header_msg "Installing prerequisites"
	winetricks --unattended dotnet40
	winetricks win7
	$WINE "$prereq_dir/NDP47-KB3186497-x86-x64-AllOS-ENU.exe" /q || true
	winetricks dxvk winxp
	cat > "$prereq_dir/txtfile.reg" <<CodeBlock
Windows Registry Editor Version 5.00
[HKEY_CLASSES_ROOT\.txt]
@="txtfile"
"Content Type"="text/plain"
[HKEY_CLASSES_ROOT\txtfile\shell\open\command]
@="winebrowser \"%1\""
CodeBlock
	[ $? == 0 ] || error_msg 7 "Failed to write *.txt file assiciation registry file to '$prereq_dir/txtfile.reg'."
	$WINE regedit.exe /S "$prereq_dir/txtfile.reg" || error_msg 5 "Failed to import '$prereq_dir/txtfile.reg' into Windows registry."
	wineserver --wait
	info_msg "Prerequisites installed."
}

function install_cosmoteer() {
	header_msg "Installing Cosmoteer"
	WINEDLLOVERRIDES="mscoree=" $WINE "$prereq_dir/Cosmoteer_Setup.exe" /NOICONS /SILENT /NOCANCEL
	info_msg "Cosmoteer installed."

}
create_64_bit_executable() {
	header_msg "Creating 64-bit launcher executable"
	cosmoteer_dir="$WINEPREFIX/drive_c/Program Files/Cosmoteer"
	cp "$cosmoteer_dir/Cosmoteer.exe.config" "$cosmoteer_dir/Cosmoteer64.exe.config"
	cat > "$WINEPREFIX/drive_c/Program Files/Cosmoteer/Cosmoteer64.cs" <<CodeBlock
using System;
using System.IO;
using System.Reflection;

class Launcher
{
	[STAThread]
	static void Main(String[] args)
	{
		var dir = AppDomain.CurrentDomain.BaseDirectory;
		Directory.SetCurrentDirectory(dir);
		Assembly assembly = Assembly.LoadFile(dir + "Cosmoteer.exe");
		Object[] argList = new Object[1];
		argList[0] = args;
		assembly.EntryPoint.Invoke(null, argList);
	}
}
CodeBlock

	$WINE "C:\\windows\\Microsoft.NET\\Framework64\\v4.0.30319\\csc.exe" /out:"C:\\Program Files\\Cosmoteer\\Cosmoteer64.exe" \
         /target:winexe \
         /platform:x64 \
         /debug- \
         /optimize+ \
         /checked- \
         /unsafe- \
         /appconfig:"C:\\Program Files\\Cosmoteer\\Cosmoteer.exe.config" \
         "C:\\Program Files\\Cosmoteer\\Cosmoteer64.cs"
  # /win32icon:"C:\\Program Files\\Cosmoteer\\Data\\icon.ico" \
	rm "$WINEPREFIX/drive_c/Program Files/Cosmoteer/Cosmoteer64.cs"
	info_msg "64-bit launcher executable created."
}


upgrade_sound() {
	header_msg "Upgrading sound to 48000 Hz"
	offset=$(( $(hexdump -ve '/1 "%02x"' "$WINEPREFIX/drive_c/Program Files/Cosmoteer/HalflingPlatformWDX.dll" | grep -bo 44ac0000 | cut -d: -f1) / 2 ))
	printf "\x80\xBB" | dd of="$WINEPREFIX/drive_c/Program Files/Cosmoteer/HalflingPlatformWDX.dll" bs=1 seek=$offset count=2 conv=notrunc
  touch "$WINEPREFIX/drive_c/Program Files/Cosmoteer/sound_upgraded"
	info_msg "Sound upgraded"
}

mkdir -p "$root"
cd "$root"

[ -d "$prereq_dir" ] || download_prerequisites
[ -d  "$WINEPREFIX" ] || prepare_wineprefix
[ -f "$WINEPREFIX/drive_c/Program Files/Cosmoteer/Cosmoteer.exe" ] || install_cosmoteer
if [[ "$WINEARCH" == "win64" ]]; then
    [ -f "$WINEPREFIX/drive_c/Program Files/Cosmoteer/Cosmoteer64.exe" ] || create_64_bit_executable
fi
[ -f "$WINEPREFIX/drive_c/Program Files/Cosmoteer/sound_upgraded" ] || upgrade_sound

rm Data || true
ln -s "$WINEPREFIX/drive_c/Program Files/Cosmoteer/Data" Data

if [[ "$WINEARCH" == "win64" ]]; then
    $WINE "C:\\Program Files\\Cosmoteer\\Cosmoteer64.exe"
else
    $WINE "C:\\Program Files\\Cosmoteer\\Cosmoteer.exe"
fi

