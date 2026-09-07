# Thunderbird + external GnuPG: signing/encrypting fails, decrypting works

Status: diagnosis (2026-07), for the symptom "Thunderbird decrypts incoming
OpenPGP mail fine, but signing and encrypting outgoing mail fails".

Confirmed on host `f13`: all identities carried
`openpgp_key_id = DFE4CD1C1843D1B27700D20F83BD958A32CA3654` and
`is_gnupg_key_id = true`, but `~/.thunderbird/default/pubring.gpg` contained
only foreign (colleague) keys — the own public key was absent, so check 3.
below failed. Fixed by importing the own public key through the OpenPGP Key
Manager; no repo change was involved in that part.

## Setup in this repo

- `modules/myconfig.email/thunderbird.nix` sets
  `programs.thunderbird.profiles.default.withExternalGnupg = true`
  (commit `807b36a87a`). Home-manager turns this into
  `mail.openpgp.allow_external_gnupg = true` and adds `gpgme` to
  `home.packages`.
- `modules/myconfig.email/default.nix` maps
  `myconfig.accounts.<n>.pgp-key-id` to the home-manager account option
  `gpg.key`. The Thunderbird home-manager module then emits, per identity:
  `mail.identity.id_<ID>.openpgp_key_id`,
  `mail.identity.id_<ID>.last_entered_external_gnupg_key_id`,
  `mail.identity.id_<ID>.is_gnupg_key_id = true`.
- `modules/gnupg.nix` configures `gpg-agent` with `pkgs.pinentry-all`.

Nothing in this repo imports any OpenPGP key into the Thunderbird profile.

## Why decrypt and sign/encrypt take different code paths

Verified against the shipped Thunderbird code
(`omni.ja`, `chrome/openpgp/content/openpgp/`):

- **Decrypt**: `modules/RNP.sys.mjs` (`RNP.decryptArray`) first tries RNP with
  Thunderbird's *internal* keyring. If RNP cannot decrypt and
  `mail.openpgp.allow_external_gnupg` is true and GPGME loaded, it falls back
  to `GPGME.decryptArray()` — i.e. to the system `gpg`/`gpg-agent`. **This path
  never consults the identity's key configuration.**
- **Sign / encrypt**: `ui/enigmailMsgComposeOverlay.js` → `prepareSendMsg()`
  reads `mail.identity.id_<ID>.openpgp_key_id` and calls
  `EnigmailEncryption.determineOwnKeyUsability()`
  (`modules/encryption.sys.mjs:242`). That function gates *both* signing and
  encrypting on:
  1. `openpgp_key_id` is non-empty for the identity actually used to send,
     otherwise the alert `cannot-send-sig-because-no-own-key` /
     `cannot-send-enc-because-no-own-key`;
  2. the key id matches `/^(0x)?[0-9a-f]+$/i` (pure hex, optional `0x`; an
     e-mail address or a fingerprint with spaces never matches);
  3. `EnigmailKeyRing.getKeyById()` finds the **public** key in Thunderbird's
     *own* keyring (`~/.thunderbird/default/pubring.gpg`). The source comment
     is explicit: *"Even for isExternalGnuPG we require that the public key is
     available."* The system `~/.gnupg` keyring is **not** used here.
     The lookup index holds key ids, short ids and full fingerprints
     (`modules/keyRing.sys.mjs:1443-1445`), so a 40-hex fingerprint in
     `openpgp_key_id` is fine — it just has to resolve to a key that is there.
  4. that public key is not revoked and not expired *in Thunderbird's copy*
     (`keyObj.getPubKeyValidity`). A stale export whose expiry was later
     extended in `gpg` counts as expired here.

  Key *acceptance* ("personal key") is **not** required in external-GnuPG mode
  (`determineOwnKeyUsability` skips that check when `isExternalGnuPG`).

Consequence: any of 1.–4. failing breaks **sign and encrypt together** while
**decryption keeps working** through the GPGME fallback. This matches the
reported symptom exactly. It also means the symptom is *not* evidence for a
broken `gpg-agent`/`pinentry`/smartcard path — the working decryption proves
`libgpgme`, `gpg` and `gpg-agent` (including the passphrase prompt) are fine.

Recipient encryption has an additional requirement: recipient public keys must
also be known to Thunderbird. Keys that only live in `~/.gnupg` are only
picked up when `mail.openpgp.fetch_pubkeys_from_gnupg = true` (Thunderbird
default: `false`; `modules/keyRing.sys.mjs:1724`). This repo now sets it in
`modules/myconfig.email/thunderbird.nix`.

## Checks to run on the affected host

```bash
P=~/.thunderbird/default

# 1. Which identity prefs did home-manager deploy, and which identity is real?
grep -E 'openpgp|is_gnupg|identity\.[^.]*\.useremail|accountmanager' \
     "$P/user.js" "$P/prefs.js"

#    -> note the identity that is actually used for sending (the one listed in
#       mail.account.account_*.identities of your real account) and check that
#       exactly THAT identity has openpgp_key_id + is_gnupg_key_id=true.
#       Home-manager names its identities `id_<hash>`; a hand-created profile
#       uses `id1`, `id2`, ... . If you send from `id1`, the home-manager prefs
#       do not apply to it.

# 2. Is the own public key in Thunderbird's keyring (NOT ~/.gnupg)?
gpg --no-default-keyring --keyring "$P/pubring.gpg" \
    --list-keys --keyid-format long
#    -> must list the key id from openpgp_key_id, not expired, not revoked.

# 3. System gpg sanity (expected to be fine if decryption works)
gpg -K --keyid-format long          # [SC] / [E] capabilities, expiry
echo test | gpg --clearsign > /dev/null && echo "system gpg sign OK"
gpg --card-status                   # only if a smartcard is involved

# 4. Verbose Thunderbird OpenPGP log for the exact error string
#    Settings > Config Editor: openpgp.loglevel = "Debug", restart, then
#    Tools > Developer > Error Console while sending.
```

## Remediation (host state, not repo state)

Depending on which check fails:

- **Own public key missing from Thunderbird's keyring** (check 2 empty):
  ```bash
  gpg --armor --export <KEYID> > /tmp/pub.asc
  ```
  Thunderbird → *Tools → OpenPGP Key Manager → File → Import Public Key(s)
  From File* → `/tmp/pub.asc`, then `rm /tmp/pub.asc`.
  Afterwards *Account Settings → End-to-End Encryption* must show the key
  under "Use your external key through GnuPG".
- **Key present but expired in Thunderbird's copy**: re-export from `gpg` and
  re-import as above; Thunderbird does not refresh it by itself.
- **Wrong identity**: either switch the account/identity to the
  home-manager-generated one, or set the key on the identity you really use
  via *Account Settings → End-to-End Encryption → Use your external key
  through GnuPG* and enter the key id.
- **`openpgp_key_id` empty or not hex**: fix
  `myconfig.accounts.<n>.pgp-key-id` in the `priv` repo to a bare hex long key
  id or fingerprint (`0x` prefix allowed, no spaces, no e-mail address) and
  re-switch.
- **Recipient keys only in `~/.gnupg`**: covered by
  `mail.openpgp.fetch_pubkeys_from_gnupg = true`; requires a re-switch and a
  Thunderbird restart. Keys found this way are offered for import per
  recipient.
