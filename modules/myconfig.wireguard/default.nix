# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT

# Option-based WireGuard mesh client module.
#
# See ./README.md for the design rationale, the topology, the roaming
# probe deep-dive, operational checks, and migration notes.
#
# Quick reference: enable on a host with
#
#   myconfig.wireguard.wg0 = {
#     enable           = true;
#     privateKeySource = ../../priv/secrets/wireguard.<host>.age;
#     # roaming = true;   # for laptops/phones (Wi-Fi-only, no fixed LAN ip)
#   };
{
  config,
  lib,
  pkgs,
  myconfig,
  ...
}:
let
  inherit (myconfig.metadatalib) get;
  metadata = get;

  # Build a full-mesh peer list for `thisHost` on the given wg interface.
  #
  # For every other host that has `wireguard.<wgInterface>.{ip4,pubkey}`:
  #   * allowedIPs = [ "<peer-wg-ip>/32" ]                       (per-peer routing)
  #   * endpoint   = peer.ip4:51820 if peer has a routable ip4   (LAN or public)
  #   * persistentKeepalive = 25 for peers that aren't on the same LAN
  #
  # The rendezvous host (`metadata.networks.<wgInterface>.peer`, typically
  # `vserver`) gets a wider `allowedIPs = [ "<wg-subnet>" ]` so it acts
  # as a catch-all router for wg peers we can't reach directly.
  #
  # When `roaming = true`, same-network peers are emitted as "ghost" peers
  # (no endpoint, empty allowedIPs). The runtime probe (see below) patches
  # them in via `wg set` when we detect we're on the home LAN.
  getWgPeersFor =
    wgInterface: thisHostName': roaming:
    let
      thisHost = metadata.hosts."${thisHostName'}";
      thisNetwork = thisHost.network or null;
      wgNetwork = metadata.networks."${wgInterface}" or { };
      rendezvousHost = wgNetwork.peer or null;
      otherHosts = lib.filter (
        { name, host }:
        name != thisHostName'
        && (lib.attrsets.hasAttrByPath [ "wireguard" wgInterface "ip4" ] host)
        && (lib.attrsets.hasAttrByPath [ "wireguard" wgInterface "pubkey" ] host)
      ) (lib.mapAttrsToList (name: host: { inherit name host; }) metadata.hosts);
    in
    lib.map (
      { name, host }:
      let
        peerNetwork = host.network or null;
        peerHasIp4 = lib.attrsets.hasAttrByPath [ "ip4" ] host;
        sameLan = thisNetwork != null && peerNetwork != null && thisNetwork == peerNetwork;
        peerWgIp = host.wireguard."${wgInterface}".ip4;
        isRendezvous = rendezvousHost != null && name == rendezvousHost;
        # Per-peer: this peer is roaming (no stable IP we can lock onto).
        # Set in metadata.json as `wireguard.<wg>.roaming = true`. We must
        # NOT bake that peer's `ip4` (a snapshot, not a reservation) into
        # an `endpoint`, because the kernel would then re-encrypt replies
        # to a stale source address that no host actually owns. WireGuard
        # endpoint roaming will discover the peer's current outer address
        # the first time it sends us a packet.
        peerIsRoaming = host.wireguard."${wgInterface}".roaming or false;
        # Local: I am roaming and this peer is on my home LAN. We can't
        # rely on my LAN ip (DHCP, foreign Wi-Fi); the runtime probe
        # patches in LAN endpoints when we detect we're at home.
        isGhostPeer = roaming && sameLan && !isRendezvous;
        # Symmetric ghosting: on a LAN-anchored host, a roaming peer's
        # static baseline is also a ghost (empty allowedIPs, no
        # endpoint) so off-road traffic to/from that peer falls through
        # to the rendezvous /24 relay in both directions. A reverse
        # roaming probe (see below) promotes it back to a direct /32
        # when a fresh direct handshake + LAN-range endpoint are seen.
        isRoamingPeerGhost = peerIsRoaming && !isRendezvous;
        allowed =
          if isGhostPeer || isRoamingPeerGhost then
            [ ]
          else if isRendezvous then
            ([ "${peerWgIp}/32" ] ++ (wgNetwork.allowedIPs or [ ]))
          else
            [ "${peerWgIp}/32" ];
        # Endpoint suppression: drop endpoint+keepalive when the peer is
        # marked roaming OR we are ghosting it locally.
        suppressEndpoint = isGhostPeer || peerIsRoaming;
      in
      {
        publicKey = host.wireguard."${wgInterface}".pubkey;
        allowedIPs = lib.unique allowed;
      }
      // (lib.optionalAttrs (peerHasIp4 && !suppressEndpoint) { endpoint = "${host.ip4}:51820"; })
      // (lib.optionalAttrs (peerHasIp4 && !sameLan && !suppressEndpoint) {
        persistentKeepalive = 25;
      })
    ) otherHosts;

  # Information needed by the roaming runtime probe: for each same-LAN
  # peer of `thisHost`, its pubkey, LAN ip:port, and wg /32 — so the
  # script can patch them in via `wg set` when we're on the home LAN.
  getWgRoamingPeersFor =
    wgInterface: thisHostName':
    let
      thisHost = metadata.hosts."${thisHostName'}";
      thisNetwork = thisHost.network or null;
      wgNetwork = metadata.networks."${wgInterface}" or { };
      rendezvousHost = wgNetwork.peer or null;
    in
    lib.filter (p: p != null) (
      lib.mapAttrsToList (
        name: host:
        let
          peerNetwork = host.network or null;
          sameLan = thisNetwork != null && peerNetwork != null && thisNetwork == peerNetwork;
          isRendezvous = rendezvousHost != null && name == rendezvousHost;
          hasWgIp = lib.attrsets.hasAttrByPath [ "wireguard" wgInterface "ip4" ] host;
          hasPubkey = lib.attrsets.hasAttrByPath [ "wireguard" wgInterface "pubkey" ] host;
          hasLanIp = lib.attrsets.hasAttrByPath [ "ip4" ] host;
          # Skip peers that are themselves roaming: their `ip4` is a
          # snapshot, not a reservation, so we shouldn't patch it in as
          # an endpoint when we come back on the home LAN.
          peerIsRoaming = host.wireguard."${wgInterface}".roaming or false;
        in
        if
          (name != thisHostName')
          && sameLan
          && !isRendezvous
          && !peerIsRoaming
          && hasWgIp
          && hasPubkey
          && hasLanIp
        then
          {
            inherit name;
            publicKey = host.wireguard."${wgInterface}".pubkey;
            wgIp = host.wireguard."${wgInterface}".ip4;
            lanIp = host.ip4;
          }
        else
          null
      ) metadata.hosts
    );

  # Reverse roaming probe input: on a non-roaming (LAN-anchored) host,
  # the set of *roaming* peers that share this host's network. Because
  # a roaming peer is now ghosted symmetrically in the static config
  # (empty allowedIPs, no endpoint), off-road traffic to it relays via
  # the rendezvous. When such a peer is actually on the home LAN we want
  # to promote it back to a direct /32 — but only after we've seen a
  # fresh direct handshake AND learned a LAN-range endpoint for it. This
  # helper returns the candidates; the runtime script does the
  # detection. We deliberately do NOT bake in a lanIp: the roaming
  # peer's ip4 is a snapshot, so the endpoint must be learned at
  # runtime via `wg show endpoints`.
  getReverseRoamingPeersFor =
    wgInterface: thisHostName':
    let
      thisHost = metadata.hosts."${thisHostName'}";
      thisNetwork = thisHost.network or null;
      wgNetwork = metadata.networks."${wgInterface}" or { };
      rendezvousHost = wgNetwork.peer or null;
    in
    lib.filter (p: p != null) (
      lib.mapAttrsToList (
        name: host:
        let
          peerNetwork = host.network or null;
          sameLan = thisNetwork != null && peerNetwork != null && thisNetwork == peerNetwork;
          isRendezvous = rendezvousHost != null && name == rendezvousHost;
          hasWgIp = lib.attrsets.hasAttrByPath [ "wireguard" wgInterface "ip4" ] host;
          hasPubkey = lib.attrsets.hasAttrByPath [ "wireguard" wgInterface "pubkey" ] host;
          peerIsRoaming = host.wireguard."${wgInterface}".roaming or false;
        in
        if (name != thisHostName') && peerIsRoaming && sameLan && !isRendezvous && hasWgIp && hasPubkey then
          {
            inherit name;
            publicKey = host.wireguard."${wgInterface}".pubkey;
            wgIp = host.wireguard."${wgInterface}".ip4;
          }
        else
          null
      ) metadata.hosts
    );

  # Bash script generator for the roaming probe. See README for details.
  #
  # Two side effects on every invocation:
  #
  #   * /run/wg-roaming/<iface>.mode  — last applied mode ("on-lan" or
  #                                     "off-lan"). The peer toggling is
  #                                     idempotent and only runs on flip.
  #   * /run/wg/<iface>.status.json   — machine-readable status snapshot
  #                                     for the waybar module + ad-hoc
  #                                     CLI inspection. Always rewritten,
  #                                     so a stale clock isn't presented.
  mkRoamingScript =
    wgInterface: roamingPeers: homeGateway:
    let
      peerEntries = lib.concatMapStringsSep " " (
        p: lib.escapeShellArg "${p.publicKey}|${p.lanIp}|${p.wgIp}"
      ) roamingPeers;
    in
    pkgs.writeShellScript "wg-roaming-${wgInterface}" ''
      set -euo pipefail
      PATH=${
        lib.makeBinPath [
          pkgs.iproute2
          pkgs.wireguard-tools
          pkgs.coreutils
          pkgs.gnugrep
        ]
      }:$PATH

      iface=${lib.escapeShellArg wgInterface}
      home_gw=${lib.escapeShellArg homeGateway}
      peers=( ${peerEntries} )

      # Detect "we are on the home LAN": the kernel's default route
      # points at the home gateway. Strict, fast, no ICMP/DNS round-trip.
      on_home_lan() {
        [[ -n "$home_gw" ]] || return 1
        ip route show default 2>/dev/null | grep -qE "via $home_gw( |$)"
      }

      write_status() {
        local mode="$1"
        local up="false"
        local peers_total=0
        local peers_recent=0
        if wg show "$iface" >/dev/null 2>&1; then
          up="true"
          peers_total=$(wg show "$iface" peers 2>/dev/null | wc -l)
          # Peers with handshake within the last 180 seconds (a freshness
          # threshold a bit over WireGuard's 120s rekey timer).
          local now
          now=$(date +%s)
          while read -r _pk hs _rest; do
            [[ -z "''${hs:-}" || "$hs" == "0" ]] && continue
            if (( now - hs < 180 )); then
              peers_recent=$(( peers_recent + 1 ))
            fi
          done < <(wg show "$iface" latest-handshakes 2>/dev/null || true)
        fi
        local status_dir=/run/wg
        mkdir -p "$status_dir"
        local tmp="$status_dir/$iface.status.json.tmp"
        cat > "$tmp" <<EOF
      {"interface":"$iface","up":$up,"mode":"$mode","roaming":true,"peers_total":$peers_total,"peers_handshake_recent":$peers_recent,"ts":$(date +%s)}
      EOF
        mv "$tmp" "$status_dir/$iface.status.json"
      }

      mode_now="off-lan"
      on_home_lan && mode_now="on-lan"

      state_dir=/run/wg-roaming
      mkdir -p "$state_dir"
      state_file="$state_dir/$iface.mode"
      mode_prev=""
      [[ -f "$state_file" ]] && mode_prev=$(cat "$state_file")

      # Idempotent peer toggling: only `wg set` on actual mode change.
      # Status file is rewritten on every run regardless, so its
      # timestamp reflects the last probe and waybar can flag staleness.
      if [[ "$mode_now" != "$mode_prev" ]]; then
        for entry in "''${peers[@]:-}"; do
          [[ -z "$entry" ]] && continue
          pubkey="''${entry%%|*}"
          rest="''${entry#*|}"
          lan_ip="''${rest%%|*}"
          wg_ip="''${rest#*|}"

          if [[ "$mode_now" == "on-lan" ]]; then
            wg set "$iface" peer "$pubkey" \
              endpoint "$lan_ip:51820" \
              allowed-ips "$wg_ip/32"
          else
            # Clear allowed-ips so traffic for this peer's wg IP falls
            # through to the rendezvous catch-all. Endpoint stays as
            # last-set; harmless without a cryptokey route to this peer.
            wg set "$iface" peer "$pubkey" allowed-ips ""
          fi
        done
        echo "$mode_now" > "$state_file"
        echo "wg-roaming[$iface]: $mode_prev -> $mode_now" >&2
      fi

      write_status "$mode_now"
    '';

  # Bash script generator for the *reverse* roaming probe, run on
  # non-roaming (LAN-anchored) hosts. For each roaming peer that shares
  # our LAN, decide whether to promote it from ghost (relay via
  # rendezvous) to direct /32:
  #
  #   * If the peer has a fresh direct handshake (age < 180s) AND its
  #     learned endpoint IP is inside our LAN CIDR -> the peer is on the
  #     home LAN right now and reachable directly:
  #         wg set <iface> peer <pk> allowed-ips <wgIp>/32
  #   * Otherwise (stale handshake, or endpoint outside the LAN, i.e.
  #     the peer is off-road and relaying via rendezvous) -> keep it a
  #     ghost so replies fall through to the rendezvous /24 catch-all:
  #         wg set <iface> peer <pk> allowed-ips ""
  #
  # Idempotent per-peer via
  # /run/wg-roaming/<iface>-reverse.<pk-sanitized> state files, mirroring
  # mkRoamingScript. The pubkey is base64 and can contain `/` and `+`,
  # which are unsafe in a filename, so we sanitize it to `[A-Za-z0-9_]`
  # before building the path. We never set an endpoint here:
  # WireGuard endpoint roaming already pins the peer's outer address
  # from its inbound packets; we only toggle the cryptokey route.
  mkReverseRoamingScript =
    wgInterface: reversePeers: lanCidr:
    let
      peerEntries = lib.concatMapStringsSep " " (
        p: lib.escapeShellArg "${p.publicKey}|${p.wgIp}"
      ) reversePeers;
    in
    pkgs.writeShellScript "wg-roaming-reverse-${wgInterface}" ''
      set -euo pipefail
      PATH=${
        lib.makeBinPath [
          pkgs.iproute2
          pkgs.wireguard-tools
          pkgs.coreutils
          pkgs.gnugrep
        ]
      }:$PATH

      iface=${lib.escapeShellArg wgInterface}
      lan_cidr=${lib.escapeShellArg lanCidr}
      peers=( ${peerEntries} )

      # Return 0 if the IPv4 address in $1 is inside the CIDR in $2.
      # Pure-bash /24-aware check that also handles arbitrary prefix
      # lengths by masking both addresses to the prefix.
      ip_in_cidr() {
        local ip="$1" cidr="$2"
        [[ -n "$ip" && -n "$cidr" ]] || return 1
        local net="''${cidr%%/*}" bits="''${cidr##*/}"
        [[ "$cidr" == */* ]] || return 1
        ip2int() {
          local a b c d
          IFS=. read -r a b c d <<<"$1" || return 1
          [[ -n "$a" && -n "$b" && -n "$c" && -n "$d" ]] || return 1
          echo $(( (a << 24) + (b << 16) + (c << 8) + d ))
        }
        local ipi neti mask
        ipi=$(ip2int "$ip") || return 1
        neti=$(ip2int "$net") || return 1
        if (( bits <= 0 )); then
          mask=0
        elif (( bits >= 32 )); then
          mask=4294967295
        else
          mask=$(( (4294967295 << (32 - bits)) & 4294967295 ))
        fi
        (( (ipi & mask) == (neti & mask) ))
      }

      now=$(date +%s)
      state_dir=/run/wg-roaming
      mkdir -p "$state_dir"

      # wg is required; if the interface isn't up yet, bail quietly.
      wg show "$iface" >/dev/null 2>&1 || exit 0

      for entry in "''${peers[@]:-}"; do
        [[ -z "$entry" ]] && continue
        pubkey="''${entry%%|*}"
        wg_ip="''${entry#*|}"

        # Handshake age for this peer.
        hs=$(wg show "$iface" latest-handshakes 2>/dev/null \
               | grep -F "$pubkey" | awk '{print $2}' || true)
        [[ -z "''${hs:-}" ]] && hs=0

        # Learned endpoint (host:port) for this peer, if any.
        ep=$(wg show "$iface" endpoints 2>/dev/null \
               | grep -F "$pubkey" | awk '{print $2}' || true)
        ep_ip="''${ep%:*}"
        # Strip brackets in case of an IPv6-literal endpoint.
        ep_ip="''${ep_ip#[}"
        ep_ip="''${ep_ip%]}"

        want="off"
        if [[ "$hs" != "0" ]] && (( now - hs < 180 )) \
             && ip_in_cidr "$ep_ip" "$lan_cidr"; then
          want="on"
        fi

        # Sanitize the base64 pubkey for use in a filename: it can
        # contain `/` (which would be treated as a directory separator,
        # breaking the write) and `+`. Map every non-alphanumeric char
        # to `_`.
        pk_safe="''${pubkey//[^A-Za-z0-9]/_}"
        state_file="$state_dir/$iface-reverse.$pk_safe"
        prev=""
        [[ -f "$state_file" ]] && prev=$(cat "$state_file")
        [[ "$want" == "$prev" ]] && continue

        if [[ "$want" == "on" ]]; then
          wg set "$iface" peer "$pubkey" allowed-ips "$wg_ip/32"
        else
          wg set "$iface" peer "$pubkey" allowed-ips ""
        fi
        echo "$want" > "$state_file"
        echo "wg-roaming-reverse[$iface]: $pubkey $prev -> $want" >&2
      done
    '';

  # Reader script used by the waybar custom module. Reads
  # /run/wg/<iface>.status.json and emits a single JSON line for waybar.
  # Only roaming hosts publish that file (the bar entry is hidden on
  # non-roaming hosts), so this script can assume the roaming shape.
  mkWaybarReader =
    wgInterface:
    pkgs.writeShellScript "wg-waybar-${wgInterface}" ''
      set -euo pipefail
      PATH=${
        lib.makeBinPath [
          pkgs.coreutils
          pkgs.jq
        ]
      }:$PATH

      iface=${lib.escapeShellArg wgInterface}
      status_file=/run/wg/$iface.status.json

      if [[ ! -r "$status_file" ]]; then
        printf '%s\n' '{"text":"wg ?","class":"warning","tooltip":"'"$iface"': no status file yet"}'
        exit 0
      fi

      jq -rc --arg iface "$iface" '
        def age: (now | floor) - .ts;
        def stale: age > 180;
        def text:
          if .up == false then "wg ✗"
          elif .roaming and .mode == "on-lan"  then "wg LAN"
          elif .roaming and .mode == "off-lan" then "wg VPN"
          else "wg ●"
          end;
        def class:
          if .up == false then "error"
          elif stale then "warning"
          elif .roaming and .mode == "off-lan" then "warning"
          else ""
          end;
        def tooltip:
          ($iface
           + ": " + (if .up then "up" else "down" end)
           + (if .roaming then " (roaming, " + .mode + ")" else "" end)
           + "\npeers: " + (.peers_handshake_recent|tostring)
           + "/" + (.peers_total|tostring) + " active"
           + "\nupdated " + (age|tostring) + "s ago"
          );
        { text: text, class: class, tooltip: tooltip }
      ' "$status_file"
    '';

  # Helper: home gateway IP for the host's `network`, used by the runtime
  # roaming probe. "" when not derivable from metadata.
  homeGatewayFor =
    thisHostName:
    let
      thisHost = metadata.hosts."${thisHostName}";
      thisNetwork = thisHost.network or null;
    in
    if
      thisNetwork != null
      && lib.attrsets.hasAttrByPath [ "networks" thisNetwork "defaultGateway" ] metadata
    then
      metadata.networks."${thisNetwork}".defaultGateway
    else
      "";

  # Helper: this host's LAN CIDR, used by the reverse roaming probe to
  # decide whether a roaming peer's learned endpoint is on our LAN.
  # Prefers an explicit `cidr` on the network; otherwise infers a /24
  # from the `defaultGateway` (a.b.c.1 -> a.b.c.0/24). "" when not
  # derivable from metadata.
  lanCidrFor =
    thisHostName:
    let
      thisHost = metadata.hosts."${thisHostName}";
      thisNetwork = thisHost.network or null;
      net = if thisNetwork != null then (metadata.networks."${thisNetwork}" or { }) else { };
      gw = net.defaultGateway or null;
      inferred =
        if gw != null then
          let
            octets = lib.splitString "." gw;
          in
          if lib.length octets == 4 then
            "${lib.elemAt octets 0}.${lib.elemAt octets 1}.${lib.elemAt octets 2}.0/24"
          else
            ""
        else
          "";
    in
    net.cidr or inferred;

  secretNameFor = wgInterface: "wireguard.private.${wgInterface}";

  # Default value of `myconfig.wireguard.<wg>.roaming` for this host:
  # read from metadata's `wireguard.<wg>.roaming`, so the truth lives in
  # one place (visible to *every* host's config eval — including peers,
  # who must know not to bake a roaming host's snapshot ip4 as a static
  # endpoint). Per-host overrides via `myconfig.wireguard.<wg>.roaming
  # = ...;` still win because the option uses `default = ...`, not
  # `mkForce`.
  metadataRoamingDefault =
    wgInterface:
    let
      thisHostMeta = metadata.hosts."${config.networking.hostName}" or { };
    in
    thisHostMeta.wireguard."${wgInterface}".roaming or false;

in
{
  options.myconfig.wireguard = lib.mkOption {
    description = ''
      Per-interface WireGuard mesh client configuration. Each attribute
      name is a wg interface (e.g. `wg0`); see ./README.md for the full
      design and the migration recipe from the old
      `metadatalib.setupAsWireguardClient` helper.
    '';
    default = { };
    type = lib.types.attrsOf (
      lib.types.submodule (
        { name, ... }:
        {
          options = {
            enable = lib.mkEnableOption "WireGuard mesh client on interface ${name}";

            privateKeySource = lib.mkOption {
              type = lib.types.nullOr lib.types.path;
              default = null;
              description = ''
                Path to the age-encrypted private key file for this host's
                WireGuard interface. Decrypted to `privateKeyFile` via the
                `myconfig.secrets` (agenix) machinery before the wg unit
                starts. Required when `enable = true`.
              '';
            };

            privateKeyFile = lib.mkOption {
              type = lib.types.str;
              default = "/etc/wireguard/${name}-private";
              description = ''
                Decrypted private key path on the running system. The
                `myconfig.secrets` entry for this interface writes the
                key here; the WireGuard unit reads from it.
              '';
            };

            roaming = lib.mkOption {
              type = lib.types.bool;
              default = metadataRoamingDefault name;
              defaultText = lib.literalExpression ''
                metadata.hosts.<thisHost>.wireguard.<wg>.roaming or false
              '';
              description = ''
                Mark this host as a roaming WireGuard client (no stable LAN
                identity, e.g. a laptop or phone). Same-LAN peers are
                emitted as "ghost" peers in the static config (no
                endpoint, empty allowedIPs), so off-LAN all wg-subnet
                traffic relays via the rendezvous host (vserver). A
                `wg-roaming-${name}` systemd service detects when the
                host is back on the home LAN and patches in direct LAN
                endpoints + per-peer /32 allowedIPs at runtime, restoring
                the LAN optimization. See README for details.

                Defaults to the value of
                `metadata.hosts.<thisHost>.wireguard.${name}.roaming`,
                so the truth lives in one place. Other hosts read that
                same metadata flag to know not to bake this host's
                snapshot ip4 as a static peer endpoint.
              '';
            };

            listenPort = lib.mkOption {
              type = lib.types.port;
              default = 51820;
              description = ''
                UDP listen port for this WireGuard interface. Stable
                across hosts so peers (LAN or via rendezvous) can find us
                and so WireGuard's endpoint roaming works.
              '';
            };

            mtu = lib.mkOption {
              type = lib.types.ints.between 576 1500;
              default = 1380;
              description = ''
                MTU of the wg interface. 1380 leaves headroom for the
                WireGuard overhead (~80 bytes) and common 1500-byte
                Ethernet underlays without fragmentation.
              '';
            };

            dnsServer = lib.mkOption {
              type = lib.types.nullOr lib.types.str;
              default = "10.199.199.1";
              description = ''
                DNS server to add to `networking.nameservers` while wg is
                enabled. Defaults to the rendezvous host's wg ip, where
                dnsmasq serves `*.${name}.maxhbr.local`. Set to `null`
                to leave the system's nameservers untouched.
              '';
            };

            waybar.enable = lib.mkOption {
              type = lib.types.bool;
              default = true;
              description = ''
                Add a `custom/wg-${name}` indicator to the waybar
                `mainBar` showing whether the interface is up and
                whether it's currently using direct LAN peering or
                relaying via the rendezvous host. Only takes effect on
                hosts where `roaming = true`; non-roaming hosts don't
                publish the status file the indicator reads. The
                reader consumes `/run/wg/${name}.status.json`, written
                on every run of the roaming probe.
              '';
            };
          };
        }
      )
    );
  };

  # Each top-level config attribute is assigned its own `lib.mkMerge`
  # over the per-interface contributions, so we never write a generic
  # `config = lib.mkMerge ...` whose *shape* depends on
  # `config.myconfig.wireguard` (which would form a self-referential
  # cycle that breaks NixOS's lazy module evaluation).
  config = {
    myconfig.secrets = lib.mkMerge (
      lib.mapAttrsToList (
        wgInterface: cfg:
        lib.mkIf cfg.enable {
          ${secretNameFor wgInterface} = {
            source = cfg.privateKeySource;
            dest = cfg.privateKeyFile;
            wantedBy = [ "wireguard-${wgInterface}.service" ];
          };
        }
      ) config.myconfig.wireguard
    );

    environment.systemPackages = lib.mkMerge (
      lib.mapAttrsToList (
        _wgInterface: cfg: lib.mkIf cfg.enable [ pkgs.wireguard-tools ]
      ) config.myconfig.wireguard
    );

    # Add this host's own wg address to /etc/hosts so that
    # `<hostname>.wg0` resolves locally (announceOtherHosts skips self).
    networking.extraHosts = lib.mkMerge (
      lib.mapAttrsToList (
        wgInterface: cfg:
        lib.mkIf cfg.enable (
          let
            thisHostWgIp = metadata.hosts."${config.networking.hostName}".wireguard."${wgInterface}".ip4;
            thisHostName = config.networking.hostName;
            baseDomain = "${wgInterface}.maxhbr.local";
          in
          ''
            ${thisHostWgIp} ${thisHostName}.${wgInterface}
            ${thisHostWgIp} ${thisHostName}.${baseDomain}
          ''
        )
      ) config.myconfig.wireguard
    );

    networking.nameservers = lib.mkMerge (
      lib.mapAttrsToList (
        _wgInterface: cfg: lib.mkIf (cfg.enable && cfg.dnsServer != null) [ cfg.dnsServer ]
      ) config.myconfig.wireguard
    );

    # NOTE: UDP/<listenPort> is intentionally NOT opened on the global
    # firewall here. Roaming hosts (laptops on untrusted Wi-Fi) must not
    # expose the wg listen port to whatever network they're on. Hosts
    # that should accept inbound wg handshakes from same-LAN peers open
    # the port on their LAN interface via `metadatalib.fixIp`, which is
    # exactly the set of LAN-anchored hosts.
    networking.wireguard.interfaces = lib.mkMerge (
      lib.mapAttrsToList (
        wgInterface: cfg:
        lib.mkIf cfg.enable {
          ${wgInterface} = {
            ips = [
              (metadata.hosts."${config.networking.hostName}".wireguard."${wgInterface}".ip4 + "/24")
            ];
            listenPort = cfg.listenPort;
            privateKeyFile = cfg.privateKeyFile;
            mtu = cfg.mtu;
            peers = getWgPeersFor wgInterface config.networking.hostName cfg.roaming;
          };
        }
      ) config.myconfig.wireguard
    );

    # Roaming probe: switches same-LAN peers between "ghost" (off-LAN,
    # all traffic relays via rendezvous) and "direct" (on-LAN, per-peer
    # /32 + endpoint). The static Nix config is the off-LAN-safe
    # baseline; this service upgrades it to LAN-direct when at home.
    # The probe also writes /run/wg/<iface>.status.json on every run;
    # the waybar custom module reads it.
    systemd.services = lib.mkMerge (
      (lib.mapAttrsToList (
        wgInterface: cfg:
        lib.mkIf (cfg.enable && cfg.roaming) {
          "wg-roaming-${wgInterface}" = {
            description = "WireGuard roaming home-LAN probe for ${wgInterface}";
            after = [
              "wireguard-${wgInterface}.service"
              "network-online.target"
            ];
            wants = [ "network-online.target" ];
            requires = [ "wireguard-${wgInterface}.service" ];
            wantedBy = [ "multi-user.target" ];
            serviceConfig = {
              Type = "oneshot";
              ExecStart =
                mkRoamingScript wgInterface (getWgRoamingPeersFor wgInterface config.networking.hostName)
                  (homeGatewayFor config.networking.hostName);
              SuccessExitStatus = [ 0 ];
            };
          };
        }
      ) config.myconfig.wireguard)
      # Reverse roaming probe: on LAN-anchored hosts that have at least
      # one roaming peer on their LAN, promote such a peer back to a
      # direct /32 when it's actually at home (fresh direct handshake +
      # LAN-range learned endpoint). See mkReverseRoamingScript.
      ++ (lib.mapAttrsToList (
        wgInterface: cfg:
        let
          reversePeers = getReverseRoamingPeersFor wgInterface config.networking.hostName;
        in
        lib.mkIf (cfg.enable && !cfg.roaming && reversePeers != [ ]) {
          "wg-roaming-reverse-${wgInterface}" = {
            description = "WireGuard reverse roaming probe for ${wgInterface}";
            after = [
              "wireguard-${wgInterface}.service"
              "network-online.target"
            ];
            wants = [ "network-online.target" ];
            requires = [ "wireguard-${wgInterface}.service" ];
            wantedBy = [ "multi-user.target" ];
            serviceConfig = {
              Type = "oneshot";
              ExecStart = mkReverseRoamingScript wgInterface reversePeers (lanCidrFor config.networking.hostName);
              SuccessExitStatus = [ 0 ];
            };
          };
        }
      ) config.myconfig.wireguard)
    );

    systemd.timers = lib.mkMerge (
      (lib.mapAttrsToList (
        wgInterface: cfg:
        lib.mkIf (cfg.enable && cfg.roaming) {
          "wg-roaming-${wgInterface}" = {
            description = "Periodic WireGuard roaming probe for ${wgInterface}";
            wantedBy = [ "timers.target" ];
            timerConfig = {
              OnBootSec = "30s";
              OnUnitActiveSec = "60s";
              AccuracySec = "5s";
              Unit = "wg-roaming-${wgInterface}.service";
            };
          };
        }
      ) config.myconfig.wireguard)
      ++ (lib.mapAttrsToList (
        wgInterface: cfg:
        let
          reversePeers = getReverseRoamingPeersFor wgInterface config.networking.hostName;
        in
        lib.mkIf (cfg.enable && !cfg.roaming && reversePeers != [ ]) {
          "wg-roaming-reverse-${wgInterface}" = {
            description = "Periodic WireGuard reverse roaming probe for ${wgInterface}";
            wantedBy = [ "timers.target" ];
            timerConfig = {
              OnBootSec = "45s";
              OnUnitActiveSec = "60s";
              AccuracySec = "5s";
              Unit = "wg-roaming-reverse-${wgInterface}.service";
            };
          };
        }
      ) config.myconfig.wireguard)
    );

    networking.networkmanager.dispatcherScripts = lib.mkMerge (
      lib.mapAttrsToList (
        wgInterface: cfg:
        lib.mkIf (cfg.enable && cfg.roaming) [
          {
            type = "basic";
            source = pkgs.writeShellScript "wg-roaming-${wgInterface}-dispatcher" ''
              case "$2" in
                up|down|connectivity-change|dhcp4-change|dhcp6-change)
                  ${pkgs.systemd}/bin/systemctl start \
                    wg-roaming-${wgInterface}.service || true
                  ;;
              esac
            '';
          }
        ]
      ) config.myconfig.wireguard
    );

    # Waybar indicator. Only roaming hosts publish a status file (the
    # roaming probe writes it on every run); on non-roaming hosts the
    # bar entry is omitted.
    #
    # Captured at module-eval time: the per-interface settings are baked
    # into the home-manager submodule below, so the HM module doesn't
    # need to re-read the system config (where `super` access can be
    # awkward and varies between user / shared-module contexts).
    home-manager.sharedModules =
      let
        waybarIfaces = lib.filter (n: n != null) (
          lib.mapAttrsToList (
            wgInterface: cfg: if cfg.enable && cfg.roaming && cfg.waybar.enable then wgInterface else null
          ) config.myconfig.wireguard
        );
      in
      lib.optional (waybarIfaces != [ ]) (
        { config, lib, ... }:
        {
          # Each enabled+roaming wg interface contributes one custom
          # waybar module + an entry in modules-right; lists merge by
          # concatenation, attrsets by key, so this composes with the
          # base waybar config in modules/myconfig.desktop.wayland/
          # programs.waybar/.
          config = lib.mkIf config.programs.waybar.enable {
            programs.waybar.settings.mainBar = lib.mkMerge (
              lib.map (wgInterface: {
                modules-right = [ "custom/wg-${wgInterface}" ];
                "custom/wg-${wgInterface}" = {
                  format = "{}";
                  exec = mkWaybarReader wgInterface;
                  return-type = "json";
                  interval = 5;
                  rotate = 90;
                  on-click = "${pkgs.systemd}/bin/systemctl start wg-roaming-${wgInterface}.service";
                  tooltip = true;
                };
              }) waybarIfaces
            );
          };
        }
      );

    assertions = lib.mkMerge (
      lib.mapAttrsToList (
        wgInterface: cfg:
        lib.mkIf cfg.enable [
          {
            assertion = lib.attrsets.hasAttrByPath [
              "wireguard"
              wgInterface
              "ip4"
            ] metadata.hosts."${config.networking.hostName}";
            message =
              "myconfig.wireguard.${wgInterface}.enable = true on host '${config.networking.hostName}', "
              + "but metadata.json has no `hosts.${config.networking.hostName}.wireguard.${wgInterface}.ip4`. "
              + "Add the wg ip4 + pubkey to hosts/metadata.json before enabling.";
          }
          {
            assertion = cfg.privateKeySource != null;
            message =
              "myconfig.wireguard.${wgInterface}.enable = true on host '${config.networking.hostName}', "
              + "but `privateKeySource` is unset. Provide an age-encrypted private key file.";
          }
          {
            # If a reverse roaming probe is wired (this host is
            # LAN-anchored and has at least one roaming peer on its
            # LAN), `lanCidrFor` must be derivable — otherwise the
            # probe's `ip_in_cidr` check always fails and the probe can
            # only ever demote, never promote (a silently broken
            # reverse probe).
            assertion =
              cfg.roaming
              || (getReverseRoamingPeersFor wgInterface config.networking.hostName) == [ ]
              || (lanCidrFor config.networking.hostName) != "";
            message =
              "myconfig.wireguard.${wgInterface} on host '${config.networking.hostName}' wires a "
              + "reverse roaming probe (a roaming peer shares this host's LAN), but no LAN CIDR is "
              + "derivable: set `metadata.networks.<net>.cidr` or a 4-octet `defaultGateway` so the "
              + "probe can decide whether a roaming peer's learned endpoint is on the LAN.";
          }
        ]
      ) config.myconfig.wireguard
    );
  };
}
