module OverlaySpec (spec) where

import Test.Hspec
import TestPrelude

spec :: Spec
spec = parallel $ inTestM $ do
  let runOverlayTest :: String -> TestM String
      runOverlayTest package = do
        tmpDir <- getTestDir
        liftIO $ do
          writeFile (tmpDir </> "visible") "this file is visible to the jail\n"
          writeFile (tmpDir </> "not-visible") "this file is not visible to the jail\n"
        runNixDrv
          [i|
            let
              pkgs = import <nixpkgs> { overlays = [ jailOverlay ]; };
              jailOverlay = final: prev:
                jail-nix.mkOverlay {
                  inherit final prev;
                  packages = combinators: with combinators; {
                    nodejs = [ (readonly #{toNixString $ tmpDir </> "visible"}) ];
                  };
                };
              testProgram = ''
                const fs = require("fs");
                const logIfFileExists = file => console.log(file, fs.existsSync(file) ? "exists" : "doesn't exist");
                logIfFileExists("visible");
                logIfFileExists("not-visible");
              '';
            in sh ''
              cd #{tmpDir}
              ${lib.getExe #{package}} -e ${lib.escapeShellArg testProgram}
            ''
          |]

  it "allows easily specifying a nixpkgs overlay" $ do
    out <- runOverlayTest "pkgs.nodejs"
    liftIO $ out `shouldBe` "visible exists\nnot-visible doesn't exist\n"

  it "allows explicitly specifying that the package is jailed" $ do
    out <- runOverlayTest "pkgs.nodejs.jailed"
    liftIO $ out `shouldBe` "visible exists\nnot-visible doesn't exist\n"

  it "exposes access to the unjailed package under `unjailed`" $ do
    out <- runOverlayTest "pkgs.nodejs.unjailed"
    liftIO $ out `shouldBe` "visible exists\nnot-visible exists\n"

  it "supports specifying additional combinators" $ do
    tmpDir <- getTestDir
    liftIO $ writeFile (tmpDir </> "file") "some file"
    [i|
      let
        pkgs = import <nixpkgs> { overlays = [ jailOverlay ]; };
        jailOverlay = final: prev:
          jail-nix.mkOverlay {
            inherit final prev;
            additionalCombinators = combinators: {
              my-combinator = combinators.readonly #{toNixString $ tmpDir </> "file"};
            };
            packages = combinators: with combinators; {
              nodejs = [ my-combinator ];
            };
          };
      in sh ''
        cd #{tmpDir}
        ${lib.getExe pkgs.nodejs} -e "console.log(require('fs').readFileSync('file', { encoding: 'utf8' }))"
      ''
    |]
      `shouldOutput` "some file\n"

  it "supports overriding base permissions" $ do
    tmpDir <- getTestDir
    liftIO $ writeFile (tmpDir </> "file") "some file"
    [i|
      let
        pkgs = import <nixpkgs> { overlays = [ jailOverlay ]; };
        jailOverlay = final: prev:
          jail-nix.mkOverlay {
            inherit final prev;
            basePermissions = combinators: [
              (combinators.readonly #{toNixString $ tmpDir </> "file"})
              (combinators.readonly "/nix/store")
            ];
            packages = combinators: with combinators; {
              nodejs = [];
            };
          };
      in sh ''
        cd #{tmpDir}
        ${lib.getExe pkgs.nodejs} -e "console.log(require('fs').readFileSync('file', { encoding: 'utf8' }))"
      ''
    |]
      `shouldOutput` "some file\n"

  it "supports overriding the bublewrap package" $ do
    tmpDir <- getTestDir
    liftIO $ writeFile (tmpDir </> "file") "some file"
    [i|
      let
        pkgs = import <nixpkgs> { overlays = [ jailOverlay ]; };
        jailOverlay = final: prev:
          jail-nix.mkOverlay {
            inherit final prev;
            bubblewrapPackage = pkgs.writeShellApplication {
              name = "my-bubblewrap";
              text = ''
                # A version of bwrap that just prints "hello" instead of
                # actually running a jail.
                echo hello
              '';
            };
            basePermissions = null;
            packages = combinators: with combinators; {
              nodejs = [];
            };
          };
      in sh ''
        cd #{tmpDir}
        ${lib.getExe pkgs.nodejs}
      ''
    |]
      `shouldOutput` "hello\n"
