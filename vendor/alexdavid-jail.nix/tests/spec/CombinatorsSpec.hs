module CombinatorsSpec (spec) where

import Control.Concurrent (threadDelay)
import Control.Monad
import Data.List.Extra
import Data.Maybe (fromJust)
import Data.String.Interpolate.Util
import GHC.IO.Exception (ExitCode (..))
import System.Directory.Extra (createDirectoryIfMissing, doesFileExist, doesPathExist)
import System.Posix (getProcessStatus, sigKILL, signalProcess)
import System.Process qualified as Process
import Test.Hspec
import TestPrelude

spec :: Spec
spec = parallel $ inTestM $ do
  describe "add-cleanup" $ do
    it "runs arbitrary logic after the jail exits" $ do
      tmpDir <- getTestDir
      [i|
        jail "test" (sh "cat /foo-in-jail") (c: [
          (c.ro-bind "foo" "/foo-in-jail")
          (c.add-runtime "echo hello > foo")
          (c.add-cleanup "rm foo")
        ])
      |]
        `shouldOutput` "hello\n"
      liftIO $ doesFileExist (tmpDir </> "foo") `shouldReturn` False

  describe "add-path" $ do
    it "prepends the passed path to $PATH" $ do
      output <-
        runNixDrv
          [i|
            jail "test" (sh ''echo "$PATH"'') (c: [
              (c.add-path "/some/new/path")
            ])
          |]
      liftIO $ output `shouldStartWith` "/some/new/path:"

  describe "add-pkg-deps" $ do
    it "sets the path" $ do
      helloPath <- evalNixExpr "toString pkgs.hello"
      output <-
        runNixDrv
          [i|
            jail "test" (sh ''echo "$PATH"'') (c: [
              (c.add-pkg-deps [ pkgs.hello ])
            ])
          |]
      liftIO $ output `shouldStartWith` helloPath <> "/bin:"

  describe "bind-pkg" $ do
    it "binds the passed nix package at the specified location" $ do
      [i|
        jail "test" (sh "cat /foo") (c: [
          (c.bind-pkg "/foo" (pkgs.writeText "foo" "bar\n"))
        ])
      |]
        `shouldOutput` "bar\n"

  describe "fwd-env" $ do
    it "forwards the passed environment variable" $ do
      withEnv "MY_ENV_VAR" "secret" $ do
        [i|
          jail "test" (sh ''echo "''${MY_ENV_VAR-unset}"'') (c: [
            (c.fwd-env "MY_ENV_VAR")
          ])
        |]
          `shouldOutput` "secret\n"

    it "allows forwarding $PATH and extending it with add-pkg-deps" $ do
      helloPath <- evalNixExpr [i| "${pkgs.hello}/bin" |]
      coreutilsPath <- evalNixExpr [i| "${pkgs.coreutils}/bin" |]
      withEnv "PATH" "foo:bar" $ do
        [i|
          jail "test" (sh ''echo "''${PATH-unset}"'') (c: [
            (c.fwd-env "PATH")
            (c.add-pkg-deps [ pkgs.hello ])
          ])
        |]
          `shouldOutput` (helloPath <> ":" <> coreutilsPath <> ":foo:bar\n")

    it "overrides $PATH to whatever is forwarded if specified after add-pkg-deps" $ do
      coreutilsPath <- evalNixExpr [i| "${pkgs.coreutils}/bin" |]
      withEnv "PATH" "foo:bar" $ do
        [i|
          jail "test" (sh ''echo "''${PATH-unset}"'') (c: [
            (c.add-pkg-deps [ pkgs.hello ])
            (c.fwd-env "PATH")
          ])
        |]
          `shouldOutput` (coreutilsPath <> ":foo:bar\n")

    it "correctly escapes variables" $ do
      withEnv "MY_ENV_VAR" "$(echo foo)" $ do
        [i|
          jail "test" (sh ''echo "''${MY_ENV_VAR-unset}"'') (c: [
            (c.fwd-env "MY_ENV_VAR")
          ])
        |]
          `shouldOutput` "$(echo foo)\n"

    it "exits non-zero if the variable isn't set at runtime" $ do
      (exitCode, _stdout, stderr) <-
        runNixDrvWithExitCode
          [i|
            jail "test" (sh ''echo ok'') (c: [
              (c.fwd-env "MY_ENV_VAR")
            ])
          |]
      liftIO $ exitCode `shouldBe` ExitFailure 1
      liftIO $ stderr `shouldContain` "MY_ENV_VAR: unbound variable"

  describe "jail-to-host-channel" $ do
    it "calls a script on the outside of the jail with an argument and returns its stdout" $ do
      tmpDir <- getTestDir
      liftIO $ writeFile (tmpDir </> "secret") "hunter2"
      [i|
        jail "test" (sh ''getsha1 "#{tmpDir </> "secret"}"'') (c: [
          (c.jail-to-host-channel "getsha1" ''
            echo "this runs outside of the jail"
            sha1sum < "$1"
          '')
        ])
      |]
        `shouldOutput` "this runs outside of the jail\nf3bbbd66a63d4bf1747940578ec3d0103530e21d  -\n"

  describe "network" $ do
    it "grants network access to the jailed program" $ do
      out <-
        runNixDrv
          [i|
            jail "test" (sh ''curl https://alexdav.id'') (c: [
              (c.add-pkg-deps [ pkgs.curl ])
              c.network
            ])
          |]
      liftIO $ out `shouldContain` "jail.nix"

    it "allows overriding the hostname with set-hostname" $ do
      [i|
        jail "test" (sh ''${pkgs.nettools}/bin/hostname'') (c: [
          (c.set-hostname "my-hostname")
          c.network
        ])
      |]
        `shouldOutput` "my-hostname\n"

    it "does not add too many bwrap args" $ do
      out <-
        runNixDrv
          [i|
            let
              jail' = jail-nix.extend {
                inherit pkgs;
                bubblewrapPackage = pkgs.writeShellApplication {
                  name = "my-bubblewrap";
                  text = ''
                    # A version of bwrap that just prints how many arguments it
                    # was passed instead of actually running a jail.
                    echo -n "$#"
                  '';
                };
                basePermissions = null;
              };
            in
              jail' "test" "unused" (c: [ c.network ])
          |]
      liftIO $ (read out :: Int) `shouldSatisfy` (< 100)

    it "works on systems using systemd-resolved" $ do
      out <-
        runNixDrvWithinVm
          [i| { services.resolved.enable = true; } |]
          [i|
            jail "test" (sh ''curl https://alexdav.id'') (c: [
              (c.add-pkg-deps [ pkgs.curl ])
              c.network
            ])
          |]
      liftIO $ out `shouldContain` "jail.nix"

  describe "no-die-with-parent" $ do
    let waitForChildPid :: Process.Pid -> TestM Process.Pid
        waitForChildPid pid = do
          children <- liftIO $ readFile ("/proc" </> show pid </> "task" </> show pid </> "children")
          if children /= ""
            then pure $ read children
            else do
              liftIO $ threadDelay 20_000
              waitForChildPid pid

    let pidExists :: Process.Pid -> TestM Bool
        pidExists = liftIO . doesPathExist . ("/proc" </>) . show

    let killProcess :: Process.Pid -> Bool -> TestM ()
        killProcess pid waitForProcessToExit = liftIO $ do
          signalProcess sigKILL pid
          when waitForProcessToExit $ void $ getProcessStatus True False pid

    it "kills all child processes when not specifying no-die-with-parent" $ do
      (_, _, _, ph) <-
        spawnNixDrv
          [i|
            let script = sh "nohup sleep inf";
            in jail "test" script null
          |]
      outerBwrapPid <- liftIO $ fromJust <$> Process.getPid ph
      innerBwrapPid <- waitForChildPid outerBwrapPid
      bashPid <- waitForChildPid innerBwrapPid
      sleepPid <- waitForChildPid bashPid
      killProcess outerBwrapPid True
      pidExists outerBwrapPid `shouldEventuallyReturn` False
      pidExists innerBwrapPid `shouldEventuallyReturn` False
      pidExists bashPid `shouldEventuallyReturn` False
      pidExists sleepPid `shouldEventuallyReturn` False

    it "does not kill child processes when specifying no-die-with-parent" $ do
      (_, _, _, ph) <-
        spawnNixDrv
          [i|
            let script = sh "nohup sleep inf";
            in jail "test" script (c: [ c.no-die-with-parent ])
          |]
      outerBwrapPid <- liftIO $ fromJust <$> Process.getPid ph
      innerBwrapPid <- waitForChildPid outerBwrapPid
      bashPid <- waitForChildPid innerBwrapPid
      sleepPid <- waitForChildPid bashPid
      killProcess outerBwrapPid True
      pidExists outerBwrapPid `shouldEventuallyReturn` False
      liftIO $ threadDelay 500_000
      pidExists innerBwrapPid `shouldReturnTestM` True
      pidExists bashPid `shouldReturnTestM` True
      pidExists sleepPid `shouldReturnTestM` True
      killProcess sleepPid False

  describe "open-urls-in-browser" $ do
    it "exposes a $BROWSER in the jail that calls $BROWSER outside of the jail" $ do
      tmpDir <- getTestDir
      void $
        runNixDrv
          [i|
            let
              fakeBrowser = sh ''
                echo "got url: $1" >> #{tmpDir </> "fake-browser"}
              '';
              jailed = jail "test" (sh ''$BROWSER "$@"'') (c: [
                c.open-urls-in-browser
              ]);
            in sh ''
              export BROWSER=${lib.getExe fakeBrowser}
              ${lib.getExe jailed} "https://example.org"
              ${lib.getExe jailed} "http://example.org"
            ''
          |]
      liftIO $
        readFile (tmpDir </> "fake-browser")
          `shouldReturn` unindent
            [i|
              got url: https://example.org
              got url: http://example.org
            |]

    it "only allows urs that start with http(s)://" $ do
      tmpDir <- getTestDir
      void $
        runNixDrv
          [i|
            let
              fakeBrowser = sh ''
                echo "got url: $1" >> #{tmpDir </> "fake-browser"}
              '';
              jailed = jail "test" (sh ''$BROWSER "$@"'') (c: [
                c.open-urls-in-browser
              ]);
            in sh ''
              export BROWSER=${lib.getExe fakeBrowser}
              ${lib.getExe jailed} "file:///some/path"
              ${lib.getExe jailed} "--headless"
              ${lib.getExe jailed} ""
              ${lib.getExe jailed}
            ''
          |]
      liftIO $ doesFileExist (tmpDir </> "fake-browser") `shouldReturn` False

  describe "readonly-paths-from-var" $ do
    forM_ [":", " "] $ \separator ->
      describe ("with separator \"" <> separator <> "\"") $ do
        it "binds all paths in the specified environment variable as read only" $ do
          tmpDir <- getTestDir
          let separator = " "
          let mkTestPath path = liftIO $ createDirectoryIfMissing True (tmpDir </> path) >> writeFile (tmpDir </> path </> "file") ""
          mkTestPath "some/path"
          mkTestPath "some/other/path"
          mkTestPath "some/final/real/path"
          mkTestPath "some/not/in/var"
          let testPaths =
                intercalate
                  separator
                  $ map
                    (tmpDir </>)
                    [ "some/path",
                      "some/non/existent/path",
                      "some/other/path",
                      "some/other/non/existent/path",
                      "some/final/real/path"
                    ]
          withEnv "TEST_PATHS" testPaths $ do
            [i|
              jail "test" (sh "${lib.getExe pkgs.tree} ${#{toNixString (tmpDir </> "some")}}") (c: [
                (c.readonly-paths-from-var "TEST_PATHS" #{toNixString separator})
              ])
            |]
              `shouldOutput` unindent
                [i|
                  #{tmpDir </> "some"}
                  |-- final
                  |   `-- real
                  |       `-- path
                  |           `-- file
                  |-- other
                  |   `-- path
                  |       `-- file
                  `-- path
                      `-- file

                  7 directories, 3 files
                |]

        it "doesn't require the var to be set" $ do
          void $
            runNixDrv
              [i|
                jail "test" (sh "true") (c: [
                  (c.readonly-paths-from-var "THIS_VAR_IS_NOT_SET" #{toNixString separator})
                ])
              |]

  describe "reset" $ do
    it "clears all set permissions - including ones set in base permissions" $ do
      [i|
        let
          jail' = jail-nix.extend {
            inherit pkgs;
            basePermissions = c: [
              c.base
              (c.readonly "/nix/store")
              (c.write-text "/test-from-base-permissions" "")
            ];
          };
        in
          jail' "test" (sh "echo /test-from-*") (c: [
            (c.write-text "/test-from-pre-reset-jail-permissions" "")
            c.reset
            c.base
            (c.readonly "/nix/store")
            (c.write-text "/test-from-post-reset-jail-permissions" "")
          ])
      |]
        `shouldOutput` "/test-from-post-reset-jail-permissions\n"

  describe "set-argv" $ do
    it "overrides argv passed to the jailed program" $ do
      [i|
        jail "test" (sh "printf '1=%s,2=%s,3=%s' \\"$@\\"") (c: [
          (c.set-argv [ "foo" "bar baz" "foo>'bar'" ])
        ])
      |]
        `shouldOutput` "1=foo,2=bar baz,3=foo>'bar'"

  describe "set-env" $ do
    it "allows setting arbitrary environment variables" $ do
      [i|
        jail "test" (sh ''echo -n "''${MY_ENV_VAR-unset}"'') (c: [
          (c.set-env "MY_ENV_VAR" "some-value")
        ])
      |]
        `shouldOutput` "some-value"

  describe "time-zone" $ do
    it "forwards timezones on nixos systems" $ do
      [i|
        let
          tz = "EST";
          fakeEtc = pkgs.runCommand "fake-etc" {} ''
            mkdir -p $out/etc
            ln -s ${pkgs.tzdata}/share/zoneinfo $out/etc/zoneinfo
          '';
          withFakeTimeZoneOnNixos = program:
            jail program.name program (c: [
              # This is a bit convoluted, but this recreates the symlink chain that is present on nixos systems
              (c.add-pkg-deps [ fakeEtc ])
              (c.unsafe-add-raw-args "--symlink ${fakeEtc}/etc /etc/static")
              (c.unsafe-add-raw-args "--symlink /etc/static/zoneinfo /etc/zoneinfo")
              (c.unsafe-add-raw-args "--symlink /etc/zoneinfo/${tz} /etc/localtime")
            ]);
        in
          withFakeTimeZoneOnNixos (
            jail "print-time-zone" (sh "date '+%Z'") (c: [
              c.time-zone
            ])
          )
      |]
        `shouldOutput` "EST\n"

    it "forwards timezones on non-nixos systems" $ do
      [i|
        let
          tz = "EST";
          withFakeTimeZoneOnNixos = program:
            jail program.name program (c: [
              (c.unsafe-add-raw-args "--ro-bind ${pkgs.tzdata}/share/zoneinfo /etc/zoneinfo")
              (c.unsafe-add-raw-args "--symlink /etc/zoneinfo/${tz} /etc/localtime")
            ]);
        in
          withFakeTimeZoneOnNixos (
            jail "print-time-zone" (sh "date '+%Z'") (c: [
              c.time-zone
            ])
          )
      |]
        `shouldOutput` "EST\n"

    it "does not fail if /etc/localtime does not exist" $ do
      [i|
        let
          fakeEtc = pkgs.runCommand "fake-etc" {} ''
            mkdir -p $out/etc
            ln -s ${pkgs.tzdata}/share/zoneinfo $out/etc/zoneinfo
          '';
          withFakeTimeZoneOnNixos = program:
            jail program.name program (c: [
              (c.add-pkg-deps [ fakeEtc ])
              (c.unsafe-add-raw-args "--symlink ${fakeEtc}/etc /etc/static")
              (c.unsafe-add-raw-args "--symlink /etc/static/zoneinfo /etc/zoneinfo")
            ]);
        in
          withFakeTimeZoneOnNixos (
            jail "print-time-zone" (sh "date '+%Z'") (c: [
              c.time-zone
            ])
          )
      |]
        `shouldOutput` "UTC\n"

  describe "try-fwd-env" $ do
    it "forwards the environment variable if set" $ do
      withEnv "MY_ENV_VAR" "secret" $ do
        [i|
          jail "test" (sh ''echo "''${MY_ENV_VAR-unset}"'') (c: [
            (c.try-fwd-env "MY_ENV_VAR")
          ])
        |]
          `shouldOutput` "secret\n"

    it "correctly escapes variables" $ do
      withEnv "MY_ENV_VAR" "$(echo foo)" $ do
        [i|
          jail "test" (sh ''echo "''${MY_ENV_VAR-unset}"'') (c: [
            (c.try-fwd-env "MY_ENV_VAR")
          ])
        |]
          `shouldOutput` "$(echo foo)\n"

    it "does not set the environment variable if it is not set" $ do
      [i|
        jail "test" (sh ''echo "''${MY_ENV_VAR-unset}"'') (c: [
          (c.try-fwd-env "MY_ENV_VAR")
        ])
      |]
        `shouldOutput` "unset\n"

  describe "try-readonly" $ do
    it "binds paths as readonly in the jail" $ do
      tmpDir <- getTestDir
      liftIO $ writeFile (tmpDir </> "my-file") "foo"
      [i|
        jail "test" (sh ''cat "#{tmpDir </> "my-file"}"'') (c: [
          (c.try-readonly "#{tmpDir </> "my-file"}")
        ])
      |]
        `shouldOutput` "foo"
      out <-
        runNixDrv
          [i|
            jail "test" (sh ''echo new-contents | tee "#{tmpDir </> "my-file"}" 2>&1 || true'') (c: [
              (c.try-readonly "#{tmpDir </> "my-file"}")
            ])
          |]
      liftIO $ out `shouldContain` "my-file: Read-only file system"

    it "does not error if the file does not exist" $ do
      [i|
        jail "test" (sh ''echo ok'') (c: [
          (c.try-readonly "/this/path/does/not/exist")
        ])
      |]
        `shouldOutput` "ok\n"

  describe "try-readwrite" $ do
    it "binds paths as readwrite in the jail" $ do
      tmpDir <- getTestDir
      liftIO $ writeFile (tmpDir </> "my-file") "foo"
      [i|
        jail "test" (sh ''cat "#{tmpDir </> "my-file"}"'') (c: [
          (c.try-readwrite "#{tmpDir </> "my-file"}")
        ])
      |]
        `shouldOutput` "foo"
      void $
        runNixDrv
          [i|
            jail "test" (sh ''echo new-contents | tee "#{tmpDir </> "my-file"}"'') (c: [
              (c.try-readwrite "#{tmpDir </> "my-file"}")
            ])
          |]
      liftIO $ readFile (tmpDir </> "my-file") `shouldReturn` "new-contents\n"

    it "does not error if the file does not exist" $ do
      [i|
        jail "test" (sh ''echo ok'') (c: [
          (c.try-readwrite "/this/path/does/not/exist")
        ])
      |]
        `shouldOutput` "ok\n"

  describe "try-ro-bind" $ do
    it "binds paths as readonly in the jail" $ do
      tmpDir <- getTestDir
      liftIO $ writeFile (tmpDir </> "my-file") "foo"
      [i|
        jail "test" (sh ''cat /path/in/jail'') (c: [
          (c.try-ro-bind "#{tmpDir </> "my-file"}" "/path/in/jail")
        ])
      |]
        `shouldOutput` "foo"
      out <-
        runNixDrv
          [i|
            jail "test" (sh ''echo new-contents | tee /path/in/jail 2>&1 || true'') (c: [
              (c.try-ro-bind "#{tmpDir </> "my-file"}" "/path/in/jail")
            ])
          |]
      liftIO $ out `shouldContain` "/path/in/jail: Read-only file system"

    it "does not error if the file does not exist" $ do
      [i|
        jail "test" (sh ''echo ok'') (c: [
          (c.try-ro-bind "/this/path/does/not/exist" "/path/in/jail")
        ])
      |]
        `shouldOutput` "ok\n"

  describe "try-rw-bind" $ do
    it "binds paths as readwrite in the jail" $ do
      tmpDir <- getTestDir
      liftIO $ writeFile (tmpDir </> "my-file") "foo"
      [i|
        jail "test" (sh ''cat /path/in/jail'') (c: [
          (c.try-rw-bind "#{tmpDir </> "my-file"}" "/path/in/jail")
        ])
      |]
        `shouldOutput` "foo"
      void $
        runNixDrv
          [i|
            jail "test" (sh ''echo new-contents | tee /path/in/jail'') (c: [
              (c.try-rw-bind "#{tmpDir </> "my-file"}" "/path/in/jail")
            ])
          |]
      liftIO $ readFile (tmpDir </> "my-file") `shouldReturn` "new-contents\n"

    it "does not error if the file does not exist" $ do
      [i|
        jail "test" (sh ''echo ok'') (c: [
          (c.try-rw-bind "/this/path/does/not/exist" "/path/in/jail")
        ])
      |]
        `shouldOutput` "ok\n"
