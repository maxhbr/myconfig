{ pkgs, ... }:
{
  imports = [
    ./home-manager
  ];
  config = {
    nixpkgs.overlays = [(self: super: with super; with super.lib; {
      # stolen from: http://chriswarbo.net/projects/nixos/useful_hacks.html
      wrap = { paths ? [], vars ? {}, file ? null, script ? null, name ? "wrap" }:
        assert file != null || script != null ||
              abort "wrap needs 'file' or 'script' argument";
        with rec {
          set  = n: v: "--set ${escapeShellArg (escapeShellArg n)} " +
                        "'\"'${escapeShellArg (escapeShellArg v)}'\"'";
          args = (map (p: "--prefix PATH : ${p}/bin") paths) ++
                (attrValues (mapAttrs set vars));
        };
        runCommand name
          {
            f           = if file == null then writeScript name script else file;
            buildInputs = [ makeWrapper ];
          }
          ''
            makeWrapper "$f" "$out" ${toString args}
          '';

      fetchGitHashless = args: stdenv.lib.overrideDerivation
        # Use a dummy hash, to appease fetchgit's assertions
        (fetchgit (args // { sha256 = hashString "sha256" args.url; }))

        # Remove the hash-checking
        (old: {
          outputHash     = null;
          outputHashAlgo = null;
          outputHashMode = null;
          sha256         = null;
        });
    })];
  };
}
