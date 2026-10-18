{ inputs, lib, ... }:

let
  agentLibFor = inputsFromArgs:
    import ../../lib {
      inherit lib;
      inputs = inputs // inputsFromArgs;
    };
in
{ config, pkgs, lib, ... }@args:
let
  cfg = config.programs.agent-skills;
  agentLib = agentLibFor (args.inputs or { });

  activeTargets = agentLib.targetsFor { targets = cfg.targets; system = pkgs.stdenv.hostPlatform.system; };
  anyTargetEnabled = lib.any (t: t.enable or false) (builtins.attrValues cfg.targets);

  linkTargets = lib.filterAttrs (_: t: t.structure == "link") activeTargets;
  syncTargets = lib.filterAttrs (_: t: t.structure != "link") activeTargets;

  syncProgram = bundle: agentLib.mkSyncProgram {
    inherit pkgs bundle;
    # Keep disabled and link target names available for allowlist validation.
    targets = lib.mapAttrs
      (_: t: t // {
        enable = t.enable && t.structure != "link";
      })
      cfg.targets;
    system = pkgs.stdenv.hostPlatform.system;
    excludePatterns = cfg.excludePatterns;
  };
in
{
  imports = [ (import ../common.nix { inherit inputs lib; }) ];

  config = lib.mkIf cfg.enable (
    let
      bundle = cfg.bundlePath;
    in
    {
      warnings = lib.optionals (!anyTargetEnabled) [
        "agent-skills: programs.agent-skills.enable is true, but no install targets are enabled. Set targets.<name>.enable = true (for example, targets.claude.enable = true)."
      ];

      home.activation.agent-skills =
        lib.mkIf (syncTargets != { }) (lib.hm.dag.entryAfter [
        "writeBoundary"
        # After linkGeneration so Home Manager removes old-generation
        # home.file links before this sync claims (and rsync --delete's) a
        # destination directory that a previous generation populated.
        "linkGeneration"
      ] ''
          ${syncProgram bundle}/bin/skills-install
        '');

      # One-time takeover for a destination populated by an older
      # generation of this or another symlink-based deployment.
      # `linkGeneration` has already removed old-generation home.file
      # links, but entries linked by the CURRENT generation (e.g. a
      # home-manager plugin living in the same skills directory) remain —
      # and those are symlinks into the Nix store, so the directory is
      # declaratively reproducible and safe to claim. Pre-mark it so the
      # sync accepts it without AGENT_SKILLS_FORCE. A destination holding
      # anything else (regular files, foreign symlinks) is left untouched:
      # the sync then fails loudly with the AGENT_SKILLS_FORCE hint rather
      # than risking user data.
      home.activation.agent-skills-takeover =
        lib.mkIf (syncTargets != { }) (lib.hm.dag.entryBetween [ "agent-skills" ] [ "linkGeneration" ]
        (''
          __agent_skills_takeover_one() {
            local name="$1" dest="$2" entries entry target
            [ -d "$dest" ] || return 0
            [ -f "$dest/.agent-skills-managed.json" ] && return 0
            shopt -s nullglob dotglob
            entries=("$dest"/*)
            [ "''${#entries[@]}" -eq 0 ] && return 0
            for entry in "''${entries[@]}"; do
              [ -L "$entry" ] || return 0
              target="$(readlink -f -- "$entry" 2>/dev/null)" || return 0
              case "$target" in
                /nix/store/*) ;;
                *) return 0 ;;
              esac
            done
            printf 'agent-skills: pre-marking %s for takeover (all entries are store symlinks)\n' "$dest" >&2
            printf '{"schemaVersion":1,"managedBy":"agent-skills-nix","mode":"global","bundle":"takeover","target":"%s","structure":"symlink-tree"}\n' "$name" \
              > "$dest/.agent-skills-managed.json"
          }
        '' + lib.concatStringsSep "\n" (
          lib.mapAttrsToList
            (name: t: ''
              __agent_skills_takeover_one "${name}" "${lib.strings.escape [ "\"" "\\" ] t.dest}"
            '')
            syncTargets
        )));
      # Note: 'link' structure type uses home.file which requires a static path relative to $HOME.
      # Shell variable expansion in dest is not supported for 'link' type.
      # Use 'symlink-tree' or 'copy-tree' structure for dynamic paths with environment variables.
      home.file = lib.mkMerge (lib.mapAttrsToList
        (name: t:
          let
            # For link type, we need a static path relative to $HOME.
            # If dest contains shell variables, warn and extract the fallback path.
            staticDest =
              let
                dest = t.dest;
              in
              if lib.strings.hasPrefix "\${" dest || lib.strings.hasPrefix "$" dest then
              # dest contains shell variables, try to extract fallback path
              # e.g., "${CLAUDE_CONFIG_DIR:-$HOME/.claude}/skills" -> ".claude/skills"
                let
                  # Extract the fallback part after :- and before }
                  fallbackMatch = builtins.match ".*:-\\$HOME/([^}]+)\\}(.*)" dest;
                in
                if fallbackMatch != null then
                  builtins.trace "agent-skills: 'link' structure does not support shell variables, using fallback path"
                    (builtins.elemAt fallbackMatch 0) + (builtins.elemAt fallbackMatch 1)
                else
                  throw "agent-skills: 'link' structure requires a static path, got '${dest}'. Use 'symlink-tree' or 'copy-tree' for dynamic paths."
              else
                dest;
          in
          {
            ${staticDest} = {
              source = cfg.targetBundlePaths.${name};
              recursive = true;
              force = true;
            };
          }
        )
        linkTargets);
    }
  );
}
