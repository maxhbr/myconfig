module JailSpec (spec) where

import Test.Hspec
import TestPrelude

spec :: Spec
spec = parallel $ inTestM $ do
  it "runs the wrapped program" $ do
    "jail \"hello\" pkgs.hello []"
      `shouldOutput` "Hello, world!\n"

  it "can be imported using callPackage" $ do
    [i|
      let
        jail-nix-without-flake = pkgs.callPackage <jail-nix> {};
        jail-without-flake = jail-nix-without-flake.init pkgs;
      in
        jail-without-flake "hello" pkgs.hello []
    |]
      `shouldOutput` "Hello, world!\n"

  it "sets a sane default path" $ do
    defaultPath <- evalNixExpr [i| "${pkgs.coreutils}/bin:/bin" |]
    [i| jail "test" (sh ''echo -n "$PATH"'') [] |]
      `shouldOutput` defaultPath

  it "it does not forward environment variables by default" $ do
    withEnv "MY_ENV_VAR" "secret" $ do
      [i| jail "test" (sh ''echo -n "''${MY_ENV_VAR-unset}"'') [] |]
        `shouldOutput` "unset"

  it "provides `sh` in the default path" $ do
    [i| jail "test" (sh ''sh -c "echo hello"'') [] |]
      `shouldOutput` "hello\n"

  describe "forwarding package overrides to the jailed derivation" $ do
    -- This is a nix expression of a package that prints out the value of an
    -- overridable `run`:
    let overrideablePackage =
          [i| pkgs.callPackage (opts: sh opts.run) { run = "printf unchanged"; } |]

    it "successfully wraps overridable packages" $ do
      [i|
        let some-pkg = #{overrideablePackage}; in
        jail "some-pkg" some-pkg []
      |]
        `shouldOutput` "unchanged"

    it "forwards overrides to the underlying package" $ do
      [i|
        let
          some-pkg = #{overrideablePackage};
          jailed-some-pkg = jail "some-pkg" some-pkg [];
        in
          jailed-some-pkg.override { run = "printf 'new message!'"; }
      |]
        `shouldOutput` "new message!"

    it "correctly sets the passed combinators on the overridden package" $ do
      [i|
        let
          some-pkg = #{overrideablePackage};
          jailed-some-pkg = jail "some-pkg" some-pkg (c: [
            (c.write-text "/foo" "foo contents")
          ]);
        in
          jailed-some-pkg.override { run = "cat /foo"; }
      |]
        `shouldOutput` "foo contents"

    it "does not set `override` if the jailed package did not have it" $ do
      jailHasOverride <-
        evalNixExpr [i| (jail "not-overridable" (sh "") []) ? override |]
      liftIO $ jailHasOverride `shouldBe` False

  it "forwards shellPath from shell packages" $ do
    jailedBashShellPath :: String <-
      evalNixExpr [i| (jail "bash" pkgs.bash []).shellPath |]
    liftIO $ jailedBashShellPath `shouldBe` "/bin/bash"

  describe "advanced configuration" $ do
    it "allows overriding base permissions shared across all jails" $ do
      tmpDir <- getTestDir
      let secretPath = tmpDir </> "secret"
      liftIO $ writeFile secretPath "hunter2"
      [i|
        let
          jail' = jail-nix.extend {
            inherit pkgs;
            basePermissions = c: [
              (c.readonly #{toNixString secretPath})
              (c.add-pkg-deps [ pkgs.coreutils ])
              (c.readonly "/nix/store")
            ];
          };
        in
          jail' "hello" (sh #{toNixString ("cat " <> secretPath)}) []
      |]
        `shouldOutput` "hunter2"

    it "allows overriding the bubblewrap package" $ do
      [i|
        let
          jail' = jail-nix.extend {
            inherit pkgs;
            bubblewrapPackage = pkgs.writeShellApplication {
              name = "my-bubblewrap";
              text = ''
                # A version of bwrap that just prints its arguments instead of
                # actually running a jail.
                echo -n "$@"
              '';
            };
            basePermissions = null;
          };
        in
          jail' "test" "some-entrypoint" []
      |]
        `shouldOutput` "--unshare-user --unshare-ipc --unshare-pid --unshare-net --unshare-uts --unshare-cgroup --new-session --die-with-parent -- some-entrypoint"

  describe "sandboxed nix store" $ do
    let checkRuntimeDepIsExposed :: Bool -> String -> TestM ()
        checkRuntimeDepIsExposed shouldBeExposed jailExpr = do
          output <-
            runNixDrv
              [i|
                let
                  buildtime = pkgs.runCommand "buildtime" {} "touch $out";
                  runtime = pkgs.runCommand "runtime" {} "touch $out";
                  dep = pkgs.runCommand "dep" {} ''
                    echo ${buildtime}
                    echo ${runtime} > $out
                  '';
                  entryScript = sh ''
                    if [ -e "$1" ]; then
                      echo "file exists"
                    else
                      echo "file doesn't exist"
                    fi
                  '';
                  jailed = (#{jailExpr}) entryScript dep;
                in
                  sh ''
                    echo "buildtime: $(${lib.getExe jailed} ${buildtime})"
                    echo "runtime: $(${lib.getExe jailed} ${runtime})"
                  ''
              |]
          liftIO $
            output
              `shouldBe` unlines
                [ "buildtime: file doesn't exist",
                  if shouldBeExposed
                    then "runtime: file exists"
                    else "runtime: file doesn't exist"
                ]

    it "does not expose unrelated paths in the nix store" $ do
      checkRuntimeDepIsExposed
        False
        [i| entryScript: _dep:
          jail "test" entryScript []
        |]

    it "exposes runtime closures of deps in the entry" $ do
      checkRuntimeDepIsExposed
        True
        [i| entryScript: dep:
          jail
            "test"
            (sh ''
              # ${dep}
              ${lib.getExe entryScript} "$@"
            '')
            []
        |]

    it "exposes runtime closures of deps in add-pkg-deps" $ do
      checkRuntimeDepIsExposed
        True
        [i| entryScript: dep:
          jail "test" entryScript (c: [
            (c.add-pkg-deps [ dep ])
          ])
        |]

    it "exposes runtime closures of deps in argv" $ do
      checkRuntimeDepIsExposed
        True
        [i| entryScript: dep:
          jail "test" entryScript (c: [
            (c.set-argv [ (c.noescape ''"$1"'') dep ])
          ])
        |]

    it "exposes runtime closures of deps in write-text string contexts" $ do
      checkRuntimeDepIsExposed
        True
        [i| entryScript: dep:
          jail "test" entryScript (c: [
            (c.write-text "/write/text/with/runtime/dep" "${dep}")
          ])
        |]
