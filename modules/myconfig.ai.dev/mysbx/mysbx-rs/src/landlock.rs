// Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
// SPDX-License-Identifier: MIT
//! The crate's Landlock binding for the `network = "proxy-only"` broker
//! (docs/design/egress.md E1/E5, bd myconfig-n8kv.1.2): the same
//! dependency-free raw-syscall idiom as [`crate::sig`], so the crate keeps
//! zero dependencies.
//!
//! Landlock's TCP rules grant a PORT, not an address, which is why the
//! broker refuses a non-loopback upstream (design decision E1) and why the
//! rule below is expressed per port. The ABI that introduced them is 4
//! (Linux 6.7); a kernel below it cannot confine the broker at all, so a
//! `proxy-only` run is refused rather than served unfenced.
//!
//! This module lives in the library rather than in the broker binary
//! because three callers need the same answers: the broker (apply the
//! fence), `mysbx doctor` (report the ABI), and the tests (prove the fence
//! against a real kernel).

use std::os::raw::c_long;

extern "C" {
    fn syscall(num: c_long, ...) -> c_long;
    fn close(fd: i32) -> i32;
    fn prctl(option: i32, ...) -> i32;
}

/// `LANDLOCK_CREATE_RULESET_VERSION`: query the ABI instead of creating
/// a ruleset.
const LANDLOCK_CREATE_RULESET_VERSION: u32 = 1;
const SYS_LANDLOCK_CREATE_RULESET: c_long = 444;
const SYS_LANDLOCK_ADD_RULE: c_long = 445;
const SYS_LANDLOCK_RESTRICT_SELF: c_long = 446;
const PR_SET_NO_NEW_PRIVS: i32 = 38;
const LANDLOCK_RULE_NET_PORT: u32 = 2;

/// `LANDLOCK_ACCESS_NET_CONNECT_TCP`: the right to `connect(2)` a TCP
/// socket. Every other access (bind, and the whole filesystem axis) is
/// left explicitly unhandled, so the rule narrows nothing else.
pub const ACCESS_CONNECT_TCP: u64 = 1 << 1;

/// The ABI that introduced TCP port rules (`LANDLOCK_ACCESS_NET_*`):
/// Linux 6.7.
pub const ABI_NET_PORTS: i64 = 4;

/// `struct landlock_ruleset_attr`, with the network field only.
#[repr(C)]
struct RulesetAttr {
    handled_access_fs: u64,
    handled_access_net: u64,
}

/// `struct landlock_net_port_attr`. `allowed_access` comes FIRST and
/// `port` second (host byte order) — part of the ABI: a swapped struct
/// makes the kernel read the port as the access bitmask and refuse the
/// rule with `EINVAL`.
#[repr(C)]
struct NetPortAttr {
    allowed_access: u64,
    port: u64,
}

/// The Landlock ABI the kernel speaks. The error carries the kernel's own
/// reason: `ENOSYS` is a kernel older than 5.13, `EOPNOTSUPP` a boot
/// parameter that turned Landlock off, `EPERM` a seccomp filter in the
/// way. Flattening them into "unsupported" would hide a host that could
/// be fixed.
pub fn abi() -> Result<i64, String> {
    let abi = unsafe {
        syscall(
            SYS_LANDLOCK_CREATE_RULESET,
            std::ptr::null::<u8>(),
            0usize,
            LANDLOCK_CREATE_RULESET_VERSION,
        )
    };
    if abi > 0 {
        Ok(abi as i64)
    } else {
        Err(format!(
            "cannot query the Landlock ABI: {}",
            std::io::Error::last_os_error()
        ))
    }
}

/// Confine THIS process (and everything it later spawns): `connect(2)` to
/// TCP is allowed on `port` and nowhere else.
///
/// Irreversible — it is the process's own fence, and there is no way back
/// short of `exec` of a non-confined process. Landlock does not restrict
/// AF_UNIX, so a listener and a Unix-socket log keep working.
pub fn confine_to_tcp_port(port: u16) -> Result<(), String> {
    let ruleset_attr = RulesetAttr {
        handled_access_fs: 0,
        handled_access_net: ACCESS_CONNECT_TCP,
    };
    let ruleset = unsafe {
        syscall(
            SYS_LANDLOCK_CREATE_RULESET,
            &ruleset_attr as *const RulesetAttr,
            std::mem::size_of::<RulesetAttr>(),
            0u32,
        )
    };
    if ruleset < 0 {
        return Err(format!(
            "cannot create the Landlock ruleset: {}",
            std::io::Error::last_os_error()
        ));
    }
    let ruleset = ruleset as i32;
    let rule = NetPortAttr {
        allowed_access: ACCESS_CONNECT_TCP,
        port: u64::from(port),
    };
    let added = unsafe {
        syscall(
            SYS_LANDLOCK_ADD_RULE,
            ruleset,
            LANDLOCK_RULE_NET_PORT,
            &rule as *const NetPortAttr,
            0u32,
        )
    };
    if added != 0 {
        let why = std::io::Error::last_os_error();
        unsafe { close(ruleset) };
        return Err(format!(
            "cannot allow TCP port {port} in the Landlock ruleset: {why}"
        ));
    }
    // `restrict_self(2)` requires no_new_privs. Setting it changes nothing
    // for a process that already dropped every privilege, and it is the
    // kernel's precondition, not a policy of ours.
    if unsafe {
        prctl(
            PR_SET_NO_NEW_PRIVS,
            1 as c_long,
            0 as c_long,
            0 as c_long,
            0 as c_long,
        )
    } != 0
    {
        let why = std::io::Error::last_os_error();
        unsafe { close(ruleset) };
        return Err(format!("cannot set no_new_privs: {why}"));
    }
    let restricted = unsafe { syscall(SYS_LANDLOCK_RESTRICT_SELF, ruleset, 0u32) };
    let why = std::io::Error::last_os_error();
    unsafe { close(ruleset) };
    if restricted != 0 {
        return Err(format!("cannot apply the Landlock ruleset: {why}"));
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn querying_the_abi_is_either_a_version_or_a_reason() {
        // Every kernel answers one of the two; neither branch may panic.
        match abi() {
            Ok(abi) => assert!(abi >= 1, "{abi}"),
            Err(reason) => assert!(reason.contains("Landlock ABI"), "{reason}"),
        }
    }

    #[test]
    fn the_net_port_attr_layout_puts_the_access_bits_first() {
        // The kernel reads `allowed_access` from offset 0 and `port` from
        // offset 8; a swapped struct is refused with EINVAL.
        assert_eq!(std::mem::size_of::<NetPortAttr>(), 16);
        assert_eq!(std::mem::size_of::<RulesetAttr>(), 16);
        let rule = NetPortAttr {
            allowed_access: ACCESS_CONNECT_TCP,
            port: 4000,
        };
        assert_eq!(
            &rule.allowed_access as *const u64 as usize,
            &rule as *const _ as usize
        );
        assert_eq!(
            &rule.port as *const u64 as usize,
            &rule as *const _ as usize + 8
        );
    }

    #[test]
    fn tcp_port_rules_arrived_in_abi_four() {
        assert_eq!(ABI_NET_PORTS, 4);
        assert_eq!(ACCESS_CONNECT_TCP, 2);
    }
}
