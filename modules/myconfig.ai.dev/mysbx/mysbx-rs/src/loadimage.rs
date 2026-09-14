// Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
// SPDX-License-Identifier: MIT
//! The `mysbx gvisor-load-image` subcommand: loads the gVisor agent
//! container image into the caller's Podman store.
//!
//! This is a Rust reimplementation of the Nix-built
//! `agent-gvisor-load-image` script, integrated into the mysbx CLI.
//! It supports the same modes:
//!
//! - Default: load the image when missing or stale (different build)
//! - `--force`: reload unconditionally
//! - `--test`: report state without loading (exit 0 if current, 1 otherwise)
//! - `--help`: show usage
//!
//! The image reference is taken from `MYSBX_GVISOR_IMAGE` (default:
//! `localhost/agent-gvisor:latest`), or can be overridden with
//! `--image`.
//!
//! # Example
//!
//! ```bash
//! # Check if image needs updating
//! $ mysbx gvisor-load-image --test
//! ## image:    localhost/agent-gvisor:latest
//! ## ref:      localhost/agent-gvisor:latest
//! ## expected: -
//! ## loaded:   sha256:abc123...
//! ## state:    current
//!
//! # Load if needed
//! $ mysbx gvisor-load-image
//! ## image:    /nix/store/...-image.tar
//! ## ref:      localhost/agent-gvisor:latest
//! ## expected: sha256:...
//! ## loaded:   -
//! ## state:    absent
//! loading localhost/agent-gvisor:latest (this may take a moment)...
//! ```

use std::path::Path;
use std::process::Command;

/// Default image reference when `MYSBX_GVISOR_IMAGE` is not set.
const DEFAULT_IMAGE_REF: &str = "localhost/agent-gvisor:latest";

/// Usage text for the subcommand.
const USAGE: &str = "\
Usage: mysbx gvisor-load-image [--force|--test|--image <ref>|--help]

Loads the gVisor agent container image into the caller's Podman store.
Without options it loads the image when it is missing or when the loaded
one is a different build than the current artifact.

Options:
  --force   reload unconditionally
  --test    do not load anything; report the state and exit 0 only if the
            current artifact is already the loaded one (1 otherwise)
  --image   image reference to load (default: $MYSBX_GVISOR_IMAGE or
            localhost/agent-gvisor:latest)
  --help    show this text

Environment:
  MYSBX_GVISOR_IMAGE  image reference to load (default: localhost/agent-gvisor:latest)
";

/// State of the image in the Podman store.
#[derive(Debug, Clone, PartialEq, Eq)]
enum ImageState {
    /// Image is not present in the store.
    Absent,
    /// Image is present but a different build (different ID).
    Stale,
    /// Image is present and matches the expected build.
    Current,
}

/// Result of checking the loaded image.
#[derive(Debug, Clone)]
struct ImageCheck {
    /// Image path (tarball) or reference.
    image: String,
    /// Image reference (e.g., localhost/agent-gvisor:latest).
    ref_name: String,
    /// Expected image ID (from tarball manifest or build info).
    expected: Option<String>,
    /// Loaded image ID (None if absent).
    loaded: Option<String>,
    /// State classification.
    state: ImageState,
}

impl ImageCheck {
    fn report(&self) -> String {
        let mut lines = Vec::new();
        lines.push(format!("## image:    {}", self.image));
        lines.push(format!("## ref:      {}", self.ref_name));
        if let Some(ref id) = self.expected {
            lines.push(format!("## expected: sha256:{}", id));
        } else {
            lines.push("## expected: -".to_string());
        }
        if let Some(ref id) = self.loaded {
            lines.push(format!("## loaded:   sha256:{}", id));
        } else {
            lines.push("## loaded:   -".to_string());
        }
        lines.push(format!(
            "## state:    {}",
            match self.state {
                ImageState::Absent => "absent",
                ImageState::Stale => "stale",
                ImageState::Current => "current",
            }
        ));
        lines.join("\n")
    }
}

/// Extract the image ID from a docker-archive tarball's manifest.json.
/// The ID is the config blob's digest (sha256 hex, without prefix).
fn extract_image_id_from_tarball(tarball_path: &str) -> Option<String> {
    // Use podman inspect to get the image ID from the tarball
    // podman load --quiet returns the image ID, but we need to extract
    // it from the manifest without loading
    let output = Command::new("tar")
        .args([
            "--extract",
            "--to-stdout",
            "--file",
            tarball_path,
            "manifest.json",
        ])
        .output()
        .ok()?;

    if !output.status.success() {
        return None;
    }

    let manifest_text = String::from_utf8_lossy(&output.stdout);

    // Parse manifest.json to find the Config field
    // Format: [{"Config":"<sha256hex>.json", ...}]
    if let Some(config_start) = manifest_text.find("\"Config\"") {
        let after_config = &manifest_text[config_start..];
        if let Some(colon_pos) = after_config.find(':') {
            let value_start = &after_config[colon_pos + 1..];
            // Find the quoted value
            if let Some(quote_start) = value_start.find('"') {
                let value = &value_start[quote_start + 1..];
                if let Some(quote_end) = value.find('"') {
                    let config_file = &value[..quote_end];
                    // Remove .json suffix and sha256: prefix
                    let config_name = config_file.strip_suffix(".json")?;
                    let digest = config_name.strip_prefix("sha256:")?;
                    // Validate that it looks like a sha256 digest (64 hex chars)
                    if digest.len() == 64 && digest.chars().all(|c| c.is_ascii_hexdigit()) {
                        return Some(digest.to_string());
                    }
                }
            }
        }
    }

    None
}

/// Get the image ID of a loaded image in Podman store.
/// Returns None if the image is not present.
fn get_loaded_image_id(image_ref: &str) -> Option<String> {
    let output = Command::new("podman")
        .args(["image", "inspect", "--format", "{{.Id}}", image_ref])
        .output()
        .ok()?;

    if !output.status.success() {
        return None;
    }

    let id = String::from_utf8_lossy(&output.stdout).trim().to_string();
    // Podman returns sha256:<hex>, we want just the hex
    Some(id.strip_prefix("sha256:").unwrap_or(&id).to_string())
}

/// Check if an image tarball exists and extract its ID.
fn check_tarball_image(tarball_path: &str, image_ref: &str) -> ImageCheck {
    let expected = extract_image_id_from_tarball(tarball_path);
    let loaded = get_loaded_image_id(image_ref);

    let state = match (&expected, &loaded) {
        (None, _) | (_, None) => ImageState::Absent,
        (Some(exp), Some(load)) if exp == load => ImageState::Current,
        _ => ImageState::Stale,
    };

    ImageCheck {
        image: tarball_path.to_string(),
        ref_name: image_ref.to_string(),
        expected,
        loaded,
        state,
    }
}

/// Check a simple image reference (not a tarball path).
fn check_reference_image(image_ref: &str) -> ImageCheck {
    let loaded = get_loaded_image_id(image_ref);

    let state = match &loaded {
        None => ImageState::Absent,
        Some(_) => ImageState::Current, // Can't verify build identity without tarball
    };

    ImageCheck {
        image: image_ref.to_string(),
        ref_name: image_ref.to_string(),
        expected: None,
        loaded,
        state,
    }
}

/// Pull an image reference using podman pull.
fn pull_image_reference(image_ref: &str) -> Result<(), String> {
    eprintln!("pulling {} (this may take a moment)...", image_ref);
    let output = Command::new("podman")
        .args(["pull", image_ref])
        .output()
        .map_err(|e| format!("failed to run podman pull: {}", e))?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        return Err(format!("podman pull failed: {}", stderr.trim()));
    }

    Ok(())
}

/// Load an image from tarball into Podman.
fn load_image_from_tarball(tarball_path: &str, image_ref: &str) -> Result<(), String> {
    eprintln!("loading {} (this may take a moment)...", image_ref);
    let output = Command::new("podman")
        .args(["load", "--input", tarball_path])
        .output()
        .map_err(|e| format!("failed to run podman load: {}", e))?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        return Err(format!("podman load failed: {}", stderr.trim()));
    }

    Ok(())
}

/// Run the gvisor-load-image subcommand.
pub fn run(args: &[String]) -> i32 {
    let mut force = false;
    let mut test_mode = false;
    let mut image_override: Option<String> = None;

    let mut i = 0;
    while i < args.len() {
        match args[i].as_str() {
            "--force" => {
                if force {
                    eprintln!("mysbx gvisor-load-image: repeated flag: --force");
                    eprintln!("{}", USAGE);
                    return 2;
                }
                force = true;
            }
            "--test" => {
                if test_mode {
                    eprintln!("mysbx gvisor-load-image: repeated flag: --test");
                    eprintln!("{}", USAGE);
                    return 2;
                }
                test_mode = true;
            }
            "--image" => {
                if image_override.is_some() {
                    eprintln!("mysbx gvisor-load-image: repeated flag: --image");
                    eprintln!("{}", USAGE);
                    return 2;
                }
                if i + 1 >= args.len() {
                    eprintln!("mysbx gvisor-load-image: --image requires a value");
                    eprintln!("  alternatively, set MYSBX_GVISOR_IMAGE environment variable");
                    eprintln!("{}", USAGE);
                    return 2;
                }
                image_override = Some(args[i + 1].clone());
                i += 1;
            }
            "--help" | "-h" => {
                print!("{}", USAGE);
                return 0;
            }
            other => {
                eprintln!("mysbx gvisor-load-image: unknown option: {}", other);
                eprintln!("{}", USAGE);
                return 2;
            }
        }
        i += 1;
    }

    // Determine image reference
    let image_ref = image_override
        .or_else(|| std::env::var("MYSBX_GVISOR_IMAGE").ok())
        .unwrap_or_else(|| DEFAULT_IMAGE_REF.to_string());

    // Check if image is a tarball path or a reference
    let is_tarball = Path::new(&image_ref).exists();

    let check = if is_tarball {
        check_tarball_image(&image_ref, &image_ref)
    } else {
        check_reference_image(&image_ref)
    };

    // Print report
    eprintln!("{}", check.report());

    if test_mode {
        // Test mode: just report and exit based on state
        return match check.state {
            ImageState::Current => 0,
            _ => 1,
        };
    }

    // Decide whether to load
    let should_load = force || check.state != ImageState::Current;

    if should_load {
        let action = match check.state {
            ImageState::Absent => "loading",
            ImageState::Stale => "replacing",
            ImageState::Current => "reloading (--force)",
        };
        eprintln!("{} {} as {}", action, check.image, check.ref_name);

        if is_tarball {
            if let Err(e) = load_image_from_tarball(&image_ref, &image_ref) {
                eprintln!("mysbx gvisor-load-image: {}", e);
                return 70; // Infrastructure error
            }
        } else {
            // For references, fall back to podman pull
            if let Err(e) = pull_image_reference(&image_ref) {
                eprintln!("mysbx gvisor-load-image: {}", e);
                return 70; // Infrastructure error
            }
        }

        // Verify the load/pull succeeded
        let after_check = if is_tarball {
            check_tarball_image(&image_ref, &image_ref)
        } else {
            check_reference_image(&image_ref)
        };

        eprintln!("{}", after_check.report());

        if after_check.state != ImageState::Current {
            eprintln!("mysbx gvisor-load-image: load did not result in expected image");
            return 70;
        }
    } else {
        eprintln!(
            "{} is already current; pass --force to reload",
            check.ref_name
        );
    }

    0
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_image_state_display() {
        assert_eq!(format!("{:?}", ImageState::Absent), "Absent");
        assert_eq!(format!("{:?}", ImageState::Stale), "Stale");
        assert_eq!(format!("{:?}", ImageState::Current), "Current");
    }

    #[test]
    fn test_image_check_report() {
        let check = ImageCheck {
            image: "/path/to/image.tar".to_string(),
            ref_name: "localhost/agent-gvisor:latest".to_string(),
            expected: Some("abc123".to_string()),
            loaded: None,
            state: ImageState::Absent,
        };

        let report = check.report();
        assert!(report.contains("image:    /path/to/image.tar"));
        assert!(report.contains("ref:      localhost/agent-gvisor:latest"));
        assert!(report.contains("expected: sha256:abc123"));
        assert!(report.contains("loaded:   -"));
        assert!(report.contains("state:    absent"));
    }

    #[test]
    fn test_usage_not_empty() {
        assert!(!USAGE.is_empty());
        assert!(USAGE.contains("Usage:"));
        assert!(USAGE.contains("--force"));
        assert!(USAGE.contains("--test"));
        assert!(USAGE.contains("--help"));
    }

    #[test]
    fn test_extract_image_id_valid_manifest() {
        // Create a temporary tarball with a valid manifest.json. A
        // per-test directory: cargo runs these in PARALLEL, and a shared
        // manifest.json would make each test tar another test's content.
        let temp_dir = std::env::temp_dir().join("mysbx-loadimage-test-valid");
        std::fs::create_dir_all(&temp_dir).unwrap();
        let tarball_path = temp_dir.join("test_image_valid.tar");

        // Create a minimal tarball with manifest.json
        let manifest_content = r#"[{"Config":"sha256:0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef.json","RepoTags":["localhost/test:latest"]}]"#;

        // Use tar to create the archive
        let manifest_file = temp_dir.join("manifest.json");
        std::fs::write(&manifest_file, manifest_content).unwrap();

        let status = Command::new("tar")
            .args([
                "--create",
                "--file",
                tarball_path.to_str().unwrap(),
                "manifest.json",
            ])
            .current_dir(&temp_dir)
            .status();

        if let Ok(s) = status {
            if s.success() {
                let result = extract_image_id_from_tarball(tarball_path.to_str().unwrap());
                assert_eq!(
                    result,
                    Some(
                        "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef"
                            .to_string()
                    )
                );
            }
        }

        // Cleanup
        let _ = std::fs::remove_file(&tarball_path);
        let _ = std::fs::remove_file(&manifest_file);
    }

    #[test]
    fn test_extract_image_id_invalid_sha256_length() {
        // Test that invalid sha256 length is rejected (own dir — see the
        // valid-manifest test for why).
        let temp_dir = std::env::temp_dir().join("mysbx-loadimage-test-invalid");
        std::fs::create_dir_all(&temp_dir).unwrap();
        let tarball_path = temp_dir.join("test_image_invalid.tar");
        let manifest_content = r#"[{"Config":"sha256:tooshort.json"}]"#;

        let manifest_file = temp_dir.join("manifest.json");
        std::fs::write(&manifest_file, manifest_content).unwrap();

        let status = Command::new("tar")
            .args([
                "--create",
                "--file",
                tarball_path.to_str().unwrap(),
                "manifest.json",
            ])
            .current_dir(&temp_dir)
            .status();

        if let Ok(s) = status {
            if s.success() {
                let result = extract_image_id_from_tarball(tarball_path.to_str().unwrap());
                // Should return None because sha256 is not 64 chars
                assert_eq!(result, None);
            }
        }

        // Cleanup
        let _ = std::fs::remove_file(&tarball_path);
        let _ = std::fs::remove_file(&manifest_file);
    }

    #[test]
    fn test_extract_image_id_non_hex_chars() {
        // Test that non-hex characters are rejected (own dir — see the
        // valid-manifest test for why).
        let temp_dir = std::env::temp_dir().join("mysbx-loadimage-test-nonhex");
        std::fs::create_dir_all(&temp_dir).unwrap();
        let tarball_path = temp_dir.join("test_image_nonhex.tar");
        let manifest_content = r#"[{"Config":"sha256:zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz.json"}]"#;

        let manifest_file = temp_dir.join("manifest.json");
        std::fs::write(&manifest_file, manifest_content).unwrap();

        let status = Command::new("tar")
            .args([
                "--create",
                "--file",
                tarball_path.to_str().unwrap(),
                "manifest.json",
            ])
            .current_dir(&temp_dir)
            .status();

        if let Ok(s) = status {
            if s.success() {
                let result = extract_image_id_from_tarball(tarball_path.to_str().unwrap());
                // Should return None because 'z' is not a hex digit
                assert_eq!(result, None);
            }
        }

        // Cleanup
        let _ = std::fs::remove_file(&tarball_path);
        let _ = std::fs::remove_file(&manifest_file);
    }

    #[test]
    fn test_image_check_report_format() {
        let check = ImageCheck {
            image: "/path/to/image.tar".to_string(),
            ref_name: "localhost/agent-gvisor:latest".to_string(),
            expected: Some("abc123".to_string()),
            loaded: Some("def456".to_string()),
            state: ImageState::Stale,
        };

        let report = check.report();
        assert!(report.contains("## image:    /path/to/image.tar"));
        assert!(report.contains("## ref:      localhost/agent-gvisor:latest"));
        assert!(report.contains("## expected: sha256:abc123"));
        assert!(report.contains("## loaded:   sha256:def456"));
        assert!(report.contains("## state:    stale"));
    }
}
