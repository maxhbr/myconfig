{ config, pkgs, lib, ... }:

with lib;

let
  keyOptionsType = types.submodule ({ config, name, ... }: {
    options.name = mkOption {
       example = "secret.txt";
       default = name;
       type = types.str;
       description = ''
         The name of the key file.
       '';
     };

    options.text = mkOption {
      example = "super secret stuff";
      default = null;
      type = types.nullOr types.str;
      description = ''
        When non-null, this designates the text that the key should contain. So if
        the key name is ``password`` and
        ``foobar`` is set here, the contents of the file
        ``destDir``/``password``
        will be ``foobar``.

        NOTE: Either ``text``, ``keyCommand`` or
        ``keyFile`` have to be set.
      '';
    };

    options.keyCommand = mkOption {
      default = null;
      example = [ "pass" "show" "secrettoken" ];
      type = types.nullOr (types.listOf types.str);
      description = ''
        When non-null, output of this command run on local machine will be
        deployed to the specified key on the target machine.  If the key name
        is
        ``password`` and ``echo secrettoken``
        is set here, the contents of the file
        ``destDir``/``password``
        deployed will equal the output of the command ``echo secrettoken``.

        This option is especially useful when you don't want to store the secrets
        inside of your NixOps deployment but rather in a well-guarded place such as an
        encrypted file. Consider using nixpkgs.password-store as storage for
        such sensitive secrets.

        NOTE: Either ``text``, ``keyCommand`` or
        ``keyFile`` have to be set.
      '';
    };
    options.keyFile = mkOption {
      default = null;
      type = types.nullOr types.path;
      apply = value: if value == null then null else toString value;
      description = ''
        When non-null, contents of the specified file will be deployed to the
        specified key on the target machine.  If the key name is
        ``password`` and ``/foo/bar`` is set
        here, the contents of the file
        ``destDir``/``password``
        deployed will be the same as local file ``/foo/bar``.

        Since no serialization/deserialization of key contents is involved, there
        are no limits on that content: null bytes, invalid Unicode,
        ``/dev/random`` output -- anything goes.

        NOTE: Either ``text``, ``keyCommand`` or
        ``keyFile`` have to be set.
      '';
    };

    options.destDir = mkOption {
      default = "/run/keys";
      type = types.path;
      description = ''
        When specified, this allows changing the destDir directory of the key
        file from its default value of ``/run/keys``.

        This directory will be created, its permissions changed to
        ``0750`` and ownership to ``root:keys``.
      '';
    };

    options.path = mkOption {
      type = types.path;
      default = "${config.destDir}/${config.name}";
      internal = true;
      description = ''
        Path to the destination of the file, a shortcut to
        ``destDir`` + / + ``name``

        Example: For key named ``foo``,
        this option would have the value ``/run/keys/foo``.
      '';
    };

    options.user = mkOption {
      default = "root";
      type = types.str;
      description = ''
        The user which will be the owner of the key file.
      '';
    };

    options.group = mkOption {
      default = "root";
      type = types.str;
      description = ''
        The group that will be set for the key file.
      '';
    };

    options.permissions = mkOption {
      default = "0600";
      example = "0640";
      type = types.str;
      description = ''
        The default permissions to set for the key file, needs to be in the
        format accepted by ``chmod(1)``.
      '';
    };
  });

  convertOldKeyType = key: val: let
    warning = "Using plain strings for `deployment.keys' is"
            + " deprecated, please use `deployment.keys.${key}.text ="
            + " \"<value>\"` instead of `deployment.keys.${key} ="
            + " \"<value>\"`.";
  in if isString val then builtins.trace warning { text = val; } else val;

  keyType = mkOptionType {
    name = "string or key options";
    check = v: isString v || keyOptionsType.check v;
    merge = loc: defs: let
      convert = def: def // {
        value = convertOldKeyType (last loc) def.value;
      };
    in keyOptionsType.merge loc (map convert defs);
    inherit (keyOptionsType) getSubOptions;
  };

in

{

  ###### interface

  options = {

    deployment.keys = mkOption {
      default = {};
      example = { password.text = "foobar"; };
      type = types.attrsOf keyType;
      apply = mapAttrs convertOldKeyType;

      description = ''

        The set of keys to be deployed to the machine.  Each attribute maps
        a key name to a file that can be accessed as
        ``destDir``/``name``,
        where ``destDir`` defaults to
        ``/run/keys``.  Thus, ``{ password.text = "foobar"; }`` causes a file
        ``destDir``/password`` to be
        created with contents ``foobar``.  The directory ``destDir`` is only
        accessible to root and the ``keys`` group, so keep in mind
        to add any users that need to have access to a particular key to this
        group.

        Each key also gets a systemd service ``<name>-key.service``
        which is active while the key is present and inactive while the key
        is absent.  Thus, ``{ password.text = "foobar"; }`` gets
        a ``password-key.service``.
      '';
    };

  };


  ###### implementation

  config = {

    assertions = (flip mapAttrsToList config.deployment.keys (key: opts: {
      assertion = (opts.text == null && opts.keyFile != null && opts.keyCommand == null) ||
                  (opts.text != null && opts.keyFile == null && opts.keyCommand == null) ||
                  (opts.text == null && opts.keyFile == null && opts.keyCommand != null);
      message = "Deployment key '${key}' must have either a 'text', 'keyCommand' or a 'keyFile' specified.";
    })) ++ (flip mapAttrsToList config.deployment.keys (key: opts: let
      dups = lib.attrNames (lib.filterAttrs (n: v: n != key && v.path == opts.path) config.deployment.keys);
    in {
      assertion = dups == [];
      message = "Deployment key '${key}' has non-unique paths, duplicated in: ${lib.concatStringsSep ", " dups}.";
    }));

    system.activationScripts.nixops-keys =
      let
        script = ''
          mkdir -p /run/keys -m 0750
          chown root:keys /run/keys

          ${concatStringsSep "\n" (flip mapAttrsToList config.deployment.keys (name: value:
              # Make sure each key has correct ownership, since the configured owning
              # user or group may not have existed when first uploaded.
              ''
                [[ -f "${value.path}" ]] && chown '${value.user}:${value.group}' "${value.path}"
              ''
          ))}
        '';
        in stringAfter [ "users" "groups" ] "source ${pkgs.writeText "setup-keys.sh" script}";

    systemd.services = (
      { nixops-keys =
        { enable = any (key: hasPrefix "/run/" key.destDir) (
            attrValues config.deployment.keys
          );
          description = "Waiting for NixOps Keys";
          wantedBy = [ "keys.target" ];
          before = [ "keys.target" ];
          unitConfig.DefaultDependencies = false; # needed to prevent a cycle
          serviceConfig.Type = "oneshot";
          serviceConfig.RemainAfterExit = true;
          script =
            ''
              while ! [ -e /run/keys/done ]; do
                sleep 0.1
              done
            '';
        };
      }
      //
      (flip mapAttrs' config.deployment.keys (name: keyCfg:
        nameValuePair "${name}-key" {
          enable = true;
          serviceConfig.TimeoutStartSec = "infinity";
          serviceConfig.Restart = "always";
          serviceConfig.RestartSec = "100ms";
          path = [ pkgs.inotifyTools ];
          preStart = ''
            (while read f; do if [ "$f" = "${keyCfg.name}" ]; then break; fi; done \
              < <(inotifywait -qm --format '%f' -e create,move ${keyCfg.destDir}) ) &

            if [[ -e "${keyCfg.path}" ]]; then
              echo 'flapped down'
              kill %1
              exit 0
            fi
            wait %1
          '';
          script = ''
            inotifywait -qq -e delete_self "${keyCfg.path}" &

            if [[ ! -e "${keyCfg.path}" ]]; then
              echo 'flapped up'
              exit 0
            fi
            wait %1
          '';
        }
      ))
    );

  };

}
