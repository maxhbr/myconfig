{
  # This is a separate flake file from the one at the root because we don't
  # want to depend on nixpkgs
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.11";

  outputs =
    { self, nixpkgs, ... }:
    let
      pkgs = import nixpkgs { system = "x86_64-linux"; };
      lib = pkgs.lib;

      repo = rec {
        rev = if self ? rev then self.rev else lib.warn "Repo is dirty, links point to main!" "main";
        shortRev = lib.substring 0 8 rev;
        urls = {
          base = "https://git.sr.ht/~alexdavid/jail.nix";
          commit = "https://git.sr.ht/~alexdavid/jail.nix/commit/${rev}";
          file = file: lineNr: "https://git.sr.ht/~alexdavid/jail.nix/tree/${rev}${file}#L${toString lineNr}";
        };
      };

      docsFileSet =
        with lib.fileset;
        toSource {
          root = ../.;
          fileset = ../lib;
        };

      formatCombinatorDoc =
        name: combinatorObj:
        let
          attrPos = builtins.unsafeGetAttrPos "impl" combinatorObj;
          file = lib.removePrefix (toString docsFileSet) attrPos.file;
        in
        ''
          ### ${name}
          <a class="source" href="${repo.urls.file file attrPos.line}">Source</a>

          **${name} :: ${combinatorObj.sig}**

          ${
            if combinatorObj ? aliases then
              "Aliases: ${lib.concatMapStringsSep ", " (alias: "`${alias}`") combinatorObj.aliases}"
            else
              ""
          }

          ${combinatorObj.doc}
        '';

      combinatorDocs =
        let
          allCombinators =
            (import (docsFileSet + /lib/combinators.nix) { inherit pkgs; } (
              throw "Docs must not depend on jail arg"
            )).docs;

          formatSection =
            filter:
            lib.pipe allCombinators [
              (lib.filterAttrs (_: filter))
              (lib.mapAttrsToList formatCombinatorDoc)
              (lib.concatStringsSep "\n\n---\n")
            ];

          hr = ''<hr style="border: 2px solid #272525">'';

          is = flag: combinator: combinator ? ${flag} && combinator.${flag};
        in
        ''
          # Combinators

          jail.nix combinators are the building blocks to create `Permission`s,
          which grant a program specific permissions at runtime.

          These permissions can be passed into the third argument to `jail`
          function, as well as
          [`basePermissions`](advanced-configuration.md#basepermissions).

          A `Permission` is a `State -> State` function where `State` is an
          internal type that is used to eventually build the final bubblewrap
          flags. The type of `State` is not part of the public API, and may change
          in the future.

          ${hr}

          ${formatSection (v: !(is "includedInBasePermissions" v || is "internal" v || is "experimental" v))}

          ${hr}

          ## Default Included Combinators

          The following combinators are enabled by default, and do not need to be
          explicitly added to your jails unless you [override
          `basePermissions`](advanced-configuration.md#basepermissions).

          ${formatSection (is "includedInBasePermissions")}

          ${hr}

          ## Experimental Combinators

          The following combinators are experimental, and may be removed, or
          have breaking changes in the future. Please reach out if you have any
          feedback on them.

          Using these combinators will emit an evaluation warning, you can
          suppress this warning with
          [suppressExperimentalWarnings](advanced-configuration.md#suppressexperimentalwarnings).

          ${formatSection (is "experimental")}
        '';

      mkdocsSettings = {
        site_name = "jail-nix";
        repo_url = repo.urls.base;
        theme = {
          name = "readthedocs";
          hljs_languages = [ "nix" ];
        };
        extra_css = [ "extra.css" ];
        # Copyright is placed directly into the page within a <p> tag.
        # Concatenating with </p><p> here is super janky, but good enough for
        # now.
        copyright = lib.concatStringsSep "</p><p>" [
          ''
            Copyright © 2025 Alex David ///
            <a rel="license" href="https://creativecommons.org/licenses/by-nc-sa/4.0/">CC BY-NC-SA 4.0</a>.
          ''
          ''
            This documentation was generated from the <a href="${repo.urls.base}">jail.nix repo</a>,
            commit <a href="${repo.urls.commit}"><code>${repo.shortRev}</code></a>.
          ''
        ];
      };
    in
    rec {
      apps.x86_64-linux.serve = {
        type = "app";
        program = lib.getExe (
          pkgs.writeShellApplication {
            name = "serve";
            runtimeInputs = [ pkgs.python3 ];
            text = ''
              cd ${packages.x86_64-linux.default}
              python3 -m http.server
            '';
          }
        );
      };

      packages.x86_64-linux.default = pkgs.runCommand "website" { buildInputs = [ pkgs.mkdocs ]; } ''
        cp -r ${./static-docs} docs
        chmod -R +w docs
        echo ${lib.escapeShellArg combinatorDocs} > docs/combinators.md
        echo ${lib.escapeShellArg (builtins.toJSON mkdocsSettings)} > mkdocs.yml
        mkdocs build
        mv site $out
      '';
    };
}
