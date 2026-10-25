# Drop the mcp-nixos nixpkgs pin

`mcp-nixos` is taken from the frozen `nixpkgs-sbomnix` input instead of the
`nixpkgs` input:

- `flake.nix`: the `nixpkgs-sbomnix` input
  (`github:nixos/nixpkgs/fe13242281a67641c0102cdb1087f0ebe586c7df`) and its
  `mkSubPkgsOverlay "nixpkgs-sbomnix"` line.
- `flake.pkgs_overrides.nix`: the `mcp-nixos = self.nixpkgs-sbomnix.mcp-nixos;`
  overlay and its row in the header table.

That input was added for `sbomnix` (see `drop-sbomnix-nixpkgs-pin.md`); this
override reuses it because it predates the `mcp` 2.x bump and therefore still
holds a coherent `fastmcp` dependency set.

## Why it is needed

At `nixpkgs` 9ae611a455b90cf061d8f332b977e387bda8e1ca the chain

```
mcp-nixos -> fastmcp -> fastmcp-slim -> mcp
```

is inconsistent: nixpkgs ships `mcp` 2.2.0, but `fastmcp-slim` 3.4.7 declares
`mcp<2.0,>=1.24.0`. Upstream nixpkgs marks the package broken accordingly
(`pkgs/development/python-modules/fastmcp-slim/default.nix`,
`broken = lib.versionAtLeast mcp.version "2"`).

Relaxing the marker via `problems.handlers.fastmcp-slim.broken = "warn"` is
**not** a fix — the build then fails for real, because `mcp` 2.x renamed
`McpError` to `MCPError`:

```
ImportError: cannot import name 'McpError' from 'mcp.shared.exceptions'. Did you mean: 'MCPError'?
```

`pkgs.mcp-nixos` fails to evaluate on every host. The build only breaks where
it is actually referenced: `tng.nix`'s `modules/opencode/default.nix`
(`command = [ (lib.getExe pkgs.mcp-nixos) ]`) registers it as a local MCP
server, which makes the `f13` toplevel fail. This repo's own
`modules/myconfig.ai.dev/programs/programs.mcp.servers.nix` has the
corresponding entry commented out, so no override is needed there.

## What to do

1. Check whether the current `nixpkgs` input carries a coherent set again —
   upstream `fastmcp` 4.x pins `mcp>=2.2.0`, so this resolves as soon as
   nixpkgs bumps `fastmcp`:

   ```bash
   nix eval --raw --impure --expr '
     let f = builtins.getFlake ("git+file:///home/mhuber/myconfig/myconfig");
     in f.inputs.nixpkgs.legacyPackages.x86_64-linux.python3Packages.fastmcp.version'
   nix eval --raw .#nixosConfigurations.test-f13.pkgs.mcp-nixos.outPath
   ```

   The override is droppable once `pkgs.mcp-nixos` evaluates and builds at the
   plain `nixpkgs` input (no `Refusing to evaluate package` error, and a
   successful `nix build .#nixosConfigurations.test-f13.pkgs.mcp-nixos`).

2. Delete the overlay, then its row in the header table of
   `flake.pkgs_overrides.nix`, then the `mkSubPkgsOverlay "nixpkgs-sbomnix"`
   line.

3. Drop the `nixpkgs-sbomnix` input itself only after `sbomnix` no longer needs
   it either — both overrides share that input, so check
   `drop-sbomnix-nixpkgs-pin.md` before removing it.

4. Verify:

   ```bash
   nix build .#nixosConfigurations.test-f13.config.system.build.toplevel
   ./build-pkg-for-host.sh mcp-nixos f13
   ```
