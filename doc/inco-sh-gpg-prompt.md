# Why `inco.sh` asks for a GPG passphrase

## TL;DR

`inco.sh` starts a fresh Chromium browser process. Chromium 152 unconditionally
asks the freedesktop Secret Service (`org.freedesktop.secrets`) for its
"Chromium Safe Storage" key at startup. On this machine that D-Bus name is
served by **`pass-secret-service`**, which answers by running
`gpg -d ~/.password-store/secret_service/<collection>/<item>.gpg`. Whenever the
`gpg-agent` passphrase cache has expired (default TTL 600 s), that `gpg` call
makes the agent pop up a **pinentry passphrase dialog**.

The prompt has nothing to do with this repository's own secrets (git-crypt,
agenix, the `../priv/` repo). `inco.sh` never invokes `git`, `agenix` or `pass`
itself.

## The exact trigger

The chain, in order:

1. `modules/myconfig.desktop.programs.chromium.nix` — the `inco` derivation
   (`pkgs.writeShellScriptBin "inco.sh"`) ends with

   ```sh
   ${chromium}/bin/chromium --incognito --user-data-dir="$user_data_dir" "$@"
   ```

   That is the only command in the script that can reach GPG. The other three
   commands (`nmcli connection show --active`, `notify-send`,
   `date | sha256sum | base64`) cannot.

2. Chromium `152.0.7977.82` (the version pinned by the `nixpkgs` flake input,
   `pkgs/applications/networking/browsers/chromium/info.json`),
   `chrome/browser/browser_process_impl.cc:1574-1592`:

   ```cpp
   #if BUILDFLAG(IS_LINUX) && BUILDFLAG(USE_DBUS)
     const auto password_store =
         cmd_line->GetSwitchValueASCII(password_manager::kPasswordStore);
     ...
     providers.emplace_back(
         /*precedence=*/10u,
         std::make_unique<os_crypt_async::FreedesktopSecretKeyProvider>(...));
   #endif
   ```

   The provider is registered on **every** Linux build with D-Bus, independent
   of the desktop environment.

3. `components/os_crypt/async/browser/freedesktop_secret_key_provider.cc:306-318`
   (same tag) selects the backend:

   ```cpp
   case base::nix::DESKTOP_ENVIRONMENT_OTHER:
   case base::nix::DESKTOP_ENVIRONMENT_CINNAMON:
   ... GNOME / XFCE / LXQT / COSMIC ...
     InitializeFreedesktopSecretService();
     break;
   ```

   `XDG_CURRENT_DESKTOP` is `niri` (or `sway` / `labwc` / `wlroots`, see
   `modules/services.dbus.nix` and `modules/myconfig.desktop.wayland.*`), none
   of which `base::nix::GetDesktopEnvironment` knows, so it returns
   `DESKTOP_ENVIRONMENT_OTHER` → Secret Service.

   Note: this is a *behaviour change* compared to older Chromium releases,
   where `DESKTOP_ENVIRONMENT_OTHER` selected `BASIC_TEXT` (the plaintext
   fallback) and never touched the Secret Service. The old
   `components/os_crypt/sync/key_storage_util_linux.cc` no longer exists.

4. Chromium then performs, over D-Bus:
   `ReadAlias("default")` → `Unlock` → `OpenSession` → `SearchItems` →
   `Unlock` → `GetSecrets`.

5. `org.freedesktop.secrets` is D-Bus-activated. Two providers ship an
   activation file on this host:

   - `~/.local/share/dbus-1/services/org.freedesktop.secrets.service` from
     `services.pass-secret-service.enable = true`
     (`modules/programs.pass/default.nix:76`, activated via the home-manager
     module `modules/services/pass-secret-service.nix`, which writes
     `xdg.dataFile."dbus-1/services/org.freedesktop.secrets.service"`).
   - `<gnome-keyring>/share/dbus-1/services/org.freedesktop.secrets.service`
     from `services.gnome.gnome-keyring.enable = true`
     (`hosts/host.f13/default.nix:386`).

   `dbus-daemon` scans `$XDG_DATA_HOME` **before** `$XDG_DATA_DIRS`, so the
   per-user `pass-secret-service` file wins and the systemd user unit
   `pass-secret-service.service` is started.

6. `pass_secret_service` answers `GetSecrets` from
   `pass_secret_service/common/pass_store.py:get_item_password()` →
   `pypass` `PasswordStore.get_decrypted_password()`
   (`pypass/passwordstore.py:77-101`):

   ```python
   gpg = subprocess.Popen([GPG_BIN, '--quiet', '--batch', '--use-agent',
                           '-d', passfile_path], ...)
   ```

   **This is the command that causes the prompt.** `--batch` only suppresses
   `gpg`'s own tty interaction; the passphrase is requested by the separate
   `gpg-agent`, which spawns `pinentry-all`
   (`modules/gnupg.nix:11`). `modules/gnupg.nix` sets no
   `default-cache-ttl`, so GnuPG's default of 600 s applies — every `inco.sh`
   started more than ten minutes after the last GPG operation prompts again.

## Is the prompt expected?

**Half expected, half accidental.**

- Expected: *any* Chromium (and Electron) start on this host does the same
  Secret Service round trip. This is not specific to `inco.sh`; `inco.sh` only
  makes it obvious because it is launched ad hoc many times a day and each run
  is a brand-new browser process (a distinct `--user-data-dir` prevents
  delegation to an already running Chromium).

- Accidental for `inco.sh` specifically: the profile is
  `--incognito`, lives in `/tmp/incoChrome_<random>` and is deleted by the
  `trap ... EXIT`. Chromium is asking for an OS-provided key to encrypt data at
  rest that is (a) not persisted by incognito mode and (b) deleted when the
  browser exits. The round trip buys nothing and, worse, lets a throwaway
  browser session unlock the user's real `pass` store.

## What was changed

`modules/myconfig.desktop.programs.chromium.nix` now passes
`--password-store=basic` in the `inco.sh` Chromium invocation. In
`freedesktop_secret_key_provider.cc:206-208` that value short-circuits the
provider (`FinalizeFailure(InitStatus::kDisabled)`) and Chromium falls back to
`os_crypt_async::PosixKeyProvider`, so no D-Bus / `pass` / `gpg` call happens.

The flag is scoped to `inco.sh` only. The regular `chromium` package, the
regular profile and every other application keep their current behaviour. No
encryption is disabled for anything that is persisted: the affected profile is
incognito and removed on exit.

## Other options (not applied)

| Option | Effect | Trade-off |
| --- | --- | --- |
| `programs.gnupg.agent.settings."default-cache-ttl"` / `max-cache-ttl` (e.g. 8 h) | Prompt appears at most once per session | Passphrase stays in agent memory much longer; does not stop the browser from reaching into the password store |
| `--password-store=gnome-libsecret` | Explicit, but selects the same backend | No change — still `pass` → `gpg` |
| Disable `services.pass-secret-service` and let `gnome-keyring` own `org.freedesktop.secrets` | Chromium uses the login keyring instead of `pass` | Loses the `pass`-backed Secret Service for all apps; the keyring then needs its own unlock |
| Set `XDG_CURRENT_DESKTOP` to something Chromium maps to KDE/GNOME | Changes backend selection | Fragile, affects portals, GTK and every other XDG consumer |
| Do nothing | — | A pinentry dialog on most `inco.sh` runs |

## Latent misconfiguration worth a follow-up

`hosts/host.f13/default.nix:386` enables the NixOS option
`services.gnome.gnome-keyring.enable`, while
`modules/programs.pass/default.nix:76` enables the home-manager option
`services.pass-secret-service.enable`. Both claim `org.freedesktop.secrets`.
The home-manager assertion in `modules/services/pass-secret-service.nix` only
checks the *home-manager* option `services.gnome-keyring.enable`, so it does not
catch this combination. Today `pass-secret-service` wins because
`$XDG_DATA_HOME` has priority in D-Bus activation, but which daemon serves a
given secret is effectively decided by startup order and file lookup
precedence. This should be made explicit — pick one Secret Service provider per
user session.
