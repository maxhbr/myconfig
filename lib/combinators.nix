pkgs: jail:
let
  inherit (pkgs) lib;
  helpers = import ./helpers.nix pkgs;

  rawCombinators =
    lib.mapAttrs
      (
        _: file:
        import file {
          inherit
            combinators
            helpers
            jail
            lib
            pkgs
            ;
        }
      )
      {
        add-cleanup = ./combinators/add-cleanup.nix;
        add-path = ./combinators/add-path.nix;
        add-pkg-deps = ./combinators/add-pkg-deps.nix;
        add-runtime = ./combinators/add-runtime.nix;
        add-seccomp = ./combinators/add-seccomp.nix;
        base = ./combinators/base.nix;
        bind-nix-store-runtime-closure = ./combinators/bind-nix-store-runtime-closure.nix;
        bind-pkg = ./combinators/bind-pkg.nix;
        camera = ./combinators/camera.nix;
        compose = ./combinators/compose.nix;
        dbus = ./combinators/dbus.nix;
        defer = ./combinators/defer.nix;
        escape = ./combinators/escape.nix;
        fake-passwd = ./combinators/fake-passwd.nix;
        fwd-env = ./combinators/fwd-env.nix;
        gpu = ./combinators/gpu.nix;
        gui = ./combinators/gui.nix;
        include-once = ./combinators/include-once.nix;
        jail-to-host-channel = ./combinators/jail-to-host-channel.nix;
        mount-cwd = ./combinators/mount-cwd.nix;
        network = ./combinators/network.nix;
        no-die-with-parent = ./combinators/no-die-with-parent.nix;
        no-new-session = ./combinators/no-new-session.nix;
        noescape = ./combinators/noescape.nix;
        notifications = ./combinators/notifications.nix;
        open-urls-in-browser = ./combinators/open-urls-in-browser.nix;
        persist-home = ./combinators/persist-home.nix;
        pipewire = ./combinators/pipewire.nix;
        pulse = ./combinators/pulse.nix;
        readonly = ./combinators/readonly.nix;
        readonly-paths-from-var = ./combinators/readonly-paths-from-var.nix;
        readonly-runtime-args = ./combinators/readonly-runtime-args.nix;
        readwrite = ./combinators/readwrite.nix;
        readwrite-runtime-args = ./combinators/readwrite-runtime-args.nix;
        reset = ./combinators/reset.nix;
        ro-bind = ./combinators/ro-bind.nix;
        runtime-deep-ro-bind = ./combinators/runtime-deep-ro-bind.nix;
        rw-bind = ./combinators/rw-bind.nix;
        set-argv = ./combinators/set-argv.nix;
        set-env = ./combinators/set-env.nix;
        set-hostname = ./combinators/set-hostname.nix;
        share-ns = ./combinators/share-ns.nix;
        time-zone = ./combinators/time-zone.nix;
        tmpfs = ./combinators/tmpfs.nix;
        try-fwd-env = ./combinators/try-fwd-env.nix;
        try-readonly = ./combinators/try-readonly.nix;
        try-readwrite = ./combinators/try-readwrite.nix;
        try-ro-bind = ./combinators/try-ro-bind.nix;
        try-rw-bind = ./combinators/try-rw-bind.nix;
        unsafe-add-raw-args = ./combinators/unsafe-add-raw-args.nix;
        unsafe-dbus = ./combinators/unsafe-dbus.nix;
        unsafe-x11 = ./combinators/unsafe-x11.nix;
        wayland = ./combinators/wayland.nix;
        wrap-entry = ./combinators/wrap-entry.nix;
        write-text = ./combinators/write-text.nix;
        xwayland = ./combinators/xwayland.nix;
      };
  combinators = lib.mapAttrs (_: combinatorObj: combinatorObj.impl) rawCombinators;
in
{
  inherit combinators;
  docs = rawCombinators;
}
