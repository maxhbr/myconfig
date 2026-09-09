# Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# myconfig.ai.microvm — the SINGLE source of truth for the deterministic
# fixed VM slot pool.
#
# Since improvement ticket 5 the pool is grouped into fixed RESOURCE CLASSES
# (`myconfig.ai.microvm.resourceClasses`), each with its own vCPU/RAM sizing and
# its own fixed number of prebuilt slots:
#
#   agent-normal-0  02:00:00:83:00:10  192.168.83.10  vm-normal-0  cid 8300
#   agent-normal-1  02:00:00:83:00:11  192.168.83.11  vm-normal-1  cid 8301
#   agent-small-0   02:00:00:83:00:12  192.168.83.12  vm-small-0   cid 8302
#   ...
#
# Everything about a slot's identity is derived from its position in the pool;
# nothing is random and nothing is generated per job (plan §4 — the pool stays
# PREBUILT). This helper is imported by default.nix (which asserts uniqueness
# and the bounds), guest.nix (which builds one microvm.nix VM per slot),
# network.nix, hostkeys.nix, job.nix and launcher.nix, so every consumer sees
# exactly the same table.
#
# Class ORDER is the alphabetical attribute order of `resourceClasses`, and the
# global index (which drives MAC / IPv4 / VSOCK CID) is assigned by walking the
# classes in that order. Consequences worth knowing:
#   * adding or resizing a class re-numbers the slots that come after it
#     alphabetically — MAC/IP/CID are stable only for a fixed class set;
#   * a slot's NAME (`agent-<class>-<i>`) is stable regardless, and the name is
#     what every host-side directory (state, hostkeys, jobs) is keyed by.
{ lib }:
rec {
  # Upper bound on the TOTAL number of slots the deterministic MAC/IPv4
  # generator can safely produce:
  #   - MAC last octet = 0x10 + i must stay a single byte  → i <= 239
  #   - IPv4 host octet = 10  + i must stay < 255           → i <= 244
  # Take the tighter bound and leave generous headroom.
  maxSlotCount = 200;

  # Base of the deterministic per-slot AF_VSOCK context id (CID). VSOCK CIDs
  # 0 (hypervisor), 1 (loopback) and 2 (host) are RESERVED, and 0xffffffff is
  # VMADDR_CID_ANY; starting at 8300 (mnemonic: the 192.168.83.0/24 agent
  # subnet) keeps every slot far away from those and from any other microVM on
  # the host — no other VM in this repo assigns a CID.
  cidBase = 8300;

  # Linux caps an interface name at 15 characters (IFNAMSIZ - 1); default.nix
  # asserts every generated TAP name against this.
  maxInterfaceNameLength = 15;

  # A single slot. `globalIndex` is the pool-wide position (drives the network
  # identities), `classIndex` the position within the class (drives the name).
  mkSlot =
    {
      class,
      classIndex,
      globalIndex,
      vcpu,
      memoryMiB,
    }:
    rec {
      index = globalIndex;
      inherit
        class
        classIndex
        vcpu
        memoryMiB
        ;
      name = "agent-${class}-${toString classIndex}";
      hostName = name;
      # Locally-administered unicast MAC (02:..). Low octet = 0x10 + index,
      # hex-formatted so it stays a valid 2-hex-digit byte for i >= 10 (e.g.
      # i=10 → ...:1a).
      mac = "02:00:00:83:00:${lib.toLower (lib.toHexString (16 + globalIndex))}";
      # Private IPv4 on the agent bridge subnet; addressing is applied by
      # network.nix (host side) and guest.nix (guest side).
      ip = "192.168.83.${toString (10 + globalIndex)}";
      # Host-side TAP interface name. microvm.nix uses the interface `id` as the
      # tap device name, and it must fit in 15 characters — hence the class name
      # rather than the full slot name (`vm-agent-normal-0` would be 17).
      tap = "vm-${class}-${toString classIndex}";
      # RESERVED per-slot AF_VSOCK context id (ticket 3 B). Part of the slot's
      # deterministic identity (asserted unique and non-reserved in
      # default.nix), so the future noninteractive control channel has a stable,
      # collision-free address per concurrently runnable slot.
      #
      # NOT yet handed to `microvm.vsock.cid`: doing so flips
      # `microvm@<slot>.service` to `Type=notify` (microvm.nix wires a
      # socat<->vsock systemd-notify bridge as soon as a CID is set), which
      # changes VM startup semantics and can only be validated by actually
      # booting a guest on KVM. It is therefore activated together with the
      # control channel that uses it, not here.
      cid = cidBase + globalIndex;
    };

  # `resourceClasses` (attrset of { count; vcpu; memoryMiB; }) → the flat,
  # deterministic slot table. Classes are walked in alphabetical order and the
  # global index runs across the whole pool.
  mkSlots =
    resourceClasses:
    let
      classNames = lib.attrNames resourceClasses;
      step =
        acc: className:
        let
          cls = resourceClasses.${className};
        in
        {
          offset = acc.offset + cls.count;
          slots =
            acc.slots
            ++ lib.genList (
              i:
              mkSlot {
                class = className;
                classIndex = i;
                globalIndex = acc.offset + i;
                vcpu = cls.vcpu;
                memoryMiB = cls.memoryMiB;
              }
            ) cls.count;
        };
    in
    (lib.foldl' step {
      offset = 0;
      slots = [ ];
    } classNames).slots;

  # Slots of one class, in class order.
  slotsOfClass = slots: className: lib.filter (s: s.class == className) slots;
}
