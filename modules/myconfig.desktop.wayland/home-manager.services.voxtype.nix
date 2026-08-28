# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{
  config,
  lib,
  pkgs,
  ...
}:

{
  config = lib.mkIf config.services.voxtype.enable {
    services.voxtype = {
      # Use the ONNX variant so the NVIDIA Parakeet engine (onnxruntime) is
      # available. The nixpkgs onnx build registers no GPU execution
      # providers, so Parakeet runs on CPU; the int8 model keeps
      # push-to-talk latency acceptable there. flake.pkgs_overrides.nix
      # carries the osd-gtk4 override this variant also needs.
      package = pkgs.voxtype-onnx;
      # parakeet-tdt-0.6b-v3-int8: quantized (~700 MB) multilingual TDT
      # model, the fastest sensible CPU choice. Swap in
      # "parakeet-tdt-0.6b-v3" (fp32, ~2.6 GB, slightly better accuracy)
      # here and in settings.parakeet.model if accuracy ever disappoints.
      loadModels = [ "parakeet-tdt-0.6b-v3-int8" ];
      settings = {
        # State file for Waybar/polybar integration. The daemon writes
        # its current state (idle/recording/transcribing) here; the
        # waybar custom/voxtype module reads it via `voxtype status`.
        state_file = "auto";
        # Push-to-talk via the ^ key (below ESC). keyd remaps that physical
        # key (grave → f13) on its virtual device (see
        # myconfig.desktop.wayland.niri); voxtype reads evdev for F13 and
        # starts recording on press, stops + transcribes on release. evdev
        # read access comes from the `input` group (see nixos.user.nix).
        # NB: this requires keyd to be running (it is, on niri hosts); on a
        # host without keyd nothing emits F13 and the hotkey is inert.
        hotkey = {
          enabled = true;
          key = "F13";
          mode = "push_to_talk";
        };
        audio = {
          device = "default";
          sample_rate = 16000;
          max_duration_secs = 60;
          feedback = {
            enabled = true;
            theme = "subtle";
            volume = 0.7; # 0.0 to 1.0
          };
        };
        # Engine selection + model. Parakeet replaces whisper.cpp: the v3
        # TDT model auto-detects among its 25 supported languages (covers
        # the previous whisper `language = [ "en" "de" ]` setup) and emits
        # punctuation natively, so spoken_punctuation stays on only for
        # explicitly dictated symbols.
        engine = "parakeet";
        parakeet = {
          model = "parakeet-tdt-0.6b-v3-int8";
        };
        output = {
          mode = "type";
          fallback_to_clipboard = true;
          # Pause media playback when recording starts.
          pre_recording_command = "${pkgs.playerctl}/bin/playerctl pause";
        };
        text = {
          # Custom word replacements applied after transcription
          # (case-insensitive, preserves word boundaries). Fix commonly
          # misheard words, brand names, and technical terms.
          replacements = {
            "vox type" = "voxtype";
            "oh marky" = "Omarchy";
          };
          # Convert spoken punctuation words into symbols — useful for
          # developers and technical writing (e.g. saying "open paren"
          # produces "(").
          spoken_punctuation = true;
        };
        meeting = {
          # Enable `voxtype meeting start/stop` for continuous
          # transcription with chunked processing and export.
          # Meetings are stored under ~/.local/share/voxtype/meetings/
          # (already persisted via cache-directories).
          enabled = true;
        };
      };
    };

    myconfig.desktop.wayland.niri.extraBinds = ''
      Mod+G hotkey-overlay-title="Voxtype (voice)" { spawn "voxtype" "record" "toggle"; }
    '';

    # The upstream home-manager voxtype service starts at
    # `default.target` and only sets WAYLAND_DISPLAY in the service
    # Environment when `services.voxtype.wayland.display` is
    # configured. On a niri session, WAYLAND_DISPLAY is imported into
    # the systemd user environment by niri *after* `default.target`
    # (login) but *before* `graphical-session.target` is activated —
    # niri calls `systemctl --user import-environment WAYLAND_DISPLAY`
    # and then notifies systemd it is ready, which activates
    # `graphical-session.target` (niri.service has `Before=` it).
    #
    # So voxtype starts too early (at login), wtype cannot connect to
    # the Wayland socket because WAYLAND_DISPLAY is not yet in the
    # user manager environment, and every output method fails
    # ("Wayland connection failed").
    #
    # Re-anchor the service to `graphical-session.target` (the same
    # target clipboard-sync uses) so it starts only after niri has
    # imported WAYLAND_DISPLAY. `WantedBy` is overridden with
    # `mkForce` to *replace* the upstream `default.target` rather
    # than merge with it — otherwise `default.target` would still try
    # to start voxtype at login (before graphical-session.target is
    # active) and the `Requisite=` would fail immediately.
    systemd.user.services.voxtype = {
      Unit = {
        After = [ "graphical-session.target" ];
        PartOf = lib.mkForce [ "graphical-session.target" ];
        Requisite = [ "graphical-session.target" ];
      };
      Install.WantedBy = lib.mkForce [ "graphical-session.target" ];
    };

    # Persist downloaded parakeet models across reboots so the
    # voxtype-model-loader service doesn't re-download them (the int8
    # model is ~700 MB, the fp32 v3 model ~2.6 GB).
    myconfig.persistence.cache-directories = [ ".local/share/voxtype/" ];

    programs.waybar = {
      settings = {
        mainBar = {
          modules-center = [ "custom/voxtype" ];
          # Extended voxtype status: shows the status icon + model
          # name, with model/device/backend in the tooltip. Requires
          # state_file = "auto" (set in the voxtype config).
          # See: https://github.com/peteonrails/voxtype#waybar-integration
          "custom/voxtype" = {
            exec = "${config.services.voxtype.package}/bin/voxtype status --follow --format json --extended";
            return-type = "json";
            format = "{} [{}]";
            format-alt = "{model}";
            tooltip = true;
            rotate = 90;
          };
        };
      };
    };

    home.packages = [
      # Toggle voxtype meeting mode. First call starts a meeting (with an
      # optional title); the next call stops it and exports the transcript
      # as Markdown (with speaker labels + timestamps) to ~/meetings/.
      #   voxtype-meeting                 # untitled meeting
      #   voxtype-meeting "Weekly standup"  # titled meeting
      (pkgs.writeShellScriptBin "voxtype-meeting" ''
        set -euo pipefail

        voxtype="${config.services.voxtype.package}/bin/voxtype"
        meetings_dir="$HOME/meetings"
        mkdir -p "$meetings_dir"

        # Sanitize a title for use in a filename: keep alnum + hyphen,
        # collapse runs of underscores, trim edges.
        sanitize() {
          echo "$1" | tr -c '[:alnum:]-' '_' | sed 's/_\{2,\}/_/g; s/^_//; s/_$//'
        }

        # `meeting status` always exits 0; detect an active meeting from
        # its output ("Meeting Status:" when running, "No meeting" when idle).
        if "$voxtype" meeting status 2>/dev/null | grep -q "^Meeting Status:"; then
          echo "Stopping meeting..."
          "$voxtype" meeting stop

          # Derive the title from the most recent meeting for the filename.
          # `meeting list --limit 1` prints the title on the 4th line.
          title="$("$voxtype" meeting list --limit 1 2>/dev/null | sed -n '4p' | tr -d '[:space:]')"
          if [[ -n "$title" ]]; then
            file="$meetings_dir/$(date +%Y-%m-%d_%H%M%S)_$(sanitize "$title").md"
          else
            file="$meetings_dir/$(date +%Y-%m-%d_%H%M%S).md"
          fi
          "$voxtype" meeting export latest \
            --format markdown --speakers --timestamps \
            --output "$file"
          echo "Exported to: $file"
        else
          title="''${1:-}"
          if [[ -n "$title" ]]; then
            echo "Starting meeting: $title"
            "$voxtype" meeting start --title "$title"
          else
            echo "Starting meeting (untitled)"
            "$voxtype" meeting start
          fi
        fi
      '')
    ];
  };
}
