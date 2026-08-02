# Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# myconfig.ai.microvm — the SINGLE source of truth for the deterministic
# fixed VM slot pool (agent-0 .. agent-<slotCount-1>).
#
# Everything about a slot's identity is derived from its integer index `i`;
# nothing is random (plan §4). This helper is imported by BOTH default.nix
# (which asserts IP/MAC uniqueness and the slot-count bound) and guest.nix
# (which actually builds one microvm.nix VM per slot), so the assertions
# always guard the exact table that builds the VMs.
#
#   agent-0  02:00:00:83:00:10  192.168.83.10  tap vm-agent-0
#   agent-1  02:00:00:83:00:11  192.168.83.11  tap vm-agent-1
#   ...
{ lib }:
rec {
  # Upper bound on the number of slots the deterministic MAC/IPv4 generator
  # can safely produce:
  #   - MAC last octet = 0x10 + i must stay a single byte  → i <= 239
  #   - IPv4 host octet = 10  + i must stay < 255           → i <= 244
  # Take the tighter bound and leave generous headroom.
  maxSlotCount = 200;

  mkSlot = i: rec {
    index = i;
    name = "agent-${toString i}";
    hostName = name;
    # Locally-administered unicast MAC (02:..). Low octet = 0x10 + index,
    # hex-formatted so it stays a valid 2-hex-digit byte for i >= 10 (e.g.
    # i=10 → ...:1a). For i in 0..9 this is byte-identical to the plan
    # table (...:10 .. ...:19).
    mac = "02:00:00:83:00:${lib.toLower (lib.toHexString (16 + i))}";
    # Private IPv4 on the agent bridge subnet; addressing is applied later by
    # network.nix. Recorded here so the slot identity is fully deterministic.
    ip = "192.168.83.${toString (10 + i)}";
    # Host-side TAP interface name (<= 15 chars). microvm.nix uses the
    # interface `id` as the tap device name.
    tap = "vm-${name}";
  };

  mkSlots = slotCount: builtins.genList mkSlot slotCount;
}
