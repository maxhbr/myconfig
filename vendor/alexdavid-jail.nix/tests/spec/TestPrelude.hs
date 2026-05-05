module TestPrelude
  ( (</>),
    TestM,
    buildNixDrvPath,
    evalNixExpr,
    forM,
    forM_,
    getTestDir,
    i,
    inTestM,
    liftIO,
    runNixDrv,
    runNixDrvWithExitCode,
    runNixDrvWithinVm,
    spawnNixDrv,
    toNixString,
    void,
    withEnv,
    lifted,
    -- expectations
    shouldEventuallyReturn,
    shouldOutput,
    shouldReturnTestM,
  )
where

import Control.Concurrent (threadDelay)
import Control.Monad
import Control.Monad.Extra (mapMaybeM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (MonadIO, MonadReader, ReaderT, ask, asks, local, runReaderT)
import Data.Aeson qualified as Aeson
import Data.Function ((&))
import Data.List.Extra
import Data.Map qualified as Map
import Data.Maybe (fromJust)
import Data.String.Conversions
import Data.String.Interpolate (i)
import GHC.IO.Exception (ExitCode)
import GHC.IO.Handle (Handle)
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (CreateProcess, ProcessHandle, createProcess)
import System.Process qualified as Process
import Test.Hspec
import Test.Hspec.Core.Spec qualified

data Context = Context
  { tmpDir :: FilePath,
    env :: Map.Map String String
  }

newtype TestM a = TestM {testMReaderT :: ReaderT Context IO a}
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadReader Context
    )

lifted :: ((a -> IO b) -> IO c) -> (a -> TestM b) -> TestM c
lifted io action = do
  ctx <- ask
  liftIO $ io $ flip runReaderT ctx . testMReaderT . action

lifted' :: (IO x -> IO y) -> TestM x -> TestM y
lifted' fn action = lifted (fn . ($ ())) (const action)

instance Example (TestM ()) where
  type Arg (TestM ()) = Context
  evaluateExample test _ hook _ = do
    hook $ runReaderT $ testMReaderT test
    pure $ Test.Hspec.Core.Spec.Result "" Test.Hspec.Core.Spec.Success

getTestDir :: TestM FilePath
getTestDir = asks tmpDir

inTestM :: SpecWith Context -> SpecWith ()
inTestM = aroundWith $ \action () -> do
  withSystemTempDirectory "jail-nix-test" $ \tmpDir -> do
    baseEnv <-
      Map.fromList
        <$> mapMaybeM
          (\(name, getVal) -> ((name,) <$>) <$> getVal)
          [ ("HOME", lookupEnv "HOME"),
            ("LANG", pure $ Just "en_US.UTF-8"),
            ("DBUS_SESSION_BUS_ADDRESS", lookupEnv "DBUS_SESSION_BUS_ADDRESS"),
            ("XDG_RUNTIME_DIR", lookupEnv "XDG_RUNTIME_DIR")
          ]
    action $
      Context
        { tmpDir,
          env = baseEnv
        }

toNixString :: String -> String
toNixString s =
  s
    & replace "\\" "\\\\"
    & replace "\"" "\\\""
    & replace "$" "\\$"
    & ("\"" <>)
    & (<> "\"")

withEnv :: String -> String -> TestM () -> TestM ()
withEnv name value = local $ \ctx -> ctx {env = Map.insert name value (env ctx)}

mkNixDrvProccess :: String -> TestM CreateProcess
mkNixDrvProccess nixDrvExpr = do
  (exe :: String, drvPath :: FilePath) <-
    evalNixExpr
      [i| let d = #{nixDrvExpr}; in [(lib.getExe d) d.drvPath] |]
  void $ buildNixDrvPath drvPath
  env <- asks env
  pure $
    (Process.proc exe [])
      { Process.env = Just $ Map.toList env,
        Process.std_in = Process.CreatePipe,
        Process.std_out = Process.CreatePipe,
        Process.std_err = Process.CreatePipe
      }

runNixDrv :: String -> TestM String
runNixDrv = liftIO . flip Process.readCreateProcess "" <=< mkNixDrvProccess

runNixDrvWithExitCode :: String -> TestM (ExitCode, String, String)
runNixDrvWithExitCode = liftIO . flip Process.readCreateProcessWithExitCode "" <=< mkNixDrvProccess

spawnNixDrv :: String -> TestM (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)
spawnNixDrv = liftIO . createProcess <=< mkNixDrvProccess

runNixDrvWithinVm :: String -> String -> TestM String
runNixDrvWithinVm vmConfig nixDrvExpr = do
  testDir <- getTestDir
  testOutputFilePath <- (</> "test-output") <$> getTestDir
  liftIO $ writeFile testOutputFilePath "---\n"
  void $
    runNixDrvWithExitCode
      [i|
        let
          system = import <nixpkgs/nixos/lib/eval-config.nix> {
            modules = [
              ({ pkgs, lib, ... }: {
                system.stateVersion = "25.05";
                virtualisation.vmVariant.virtualisation = {
                  graphics = false;
                  sharedDirectories.testOutput = { source = #{toNixString testDir}; target = #{toNixString testDir}; };

                };
                systemd.services.run-jail-nix-vm-test = {
                  wantedBy = [ "multi-user.target" ];
                  after = [ "network-online.target" ];
                  wants = [ "network-online.target" ];
                  environment = {
                    HOME = "/root";
                  };
                  script = let
                    jail-nix = import <jail-nix> {};
                    jail = jail-nix.extend { inherit pkgs; };
                    testToRun = #{nixDrvExpr};
                  in ''
                    (
                      ${lib.getExe testToRun} || echo 'Test failed'
                    ) >${#{toNixString testOutputFilePath}} 2>&1
                    shutdown now
                  '';
                  serviceConfig.type = "oneshot";
                };
              })
              #{vmConfig}
            ];
          };
        in sh ''
          TMP_DIR=$(${pkgs.coreutils}/bin/mktemp -d)
          NIX_DISK_IMAGE="$TMP_DIR/img" ${system.config.system.build.vm}/bin/run-nixos-vm
          ${pkgs.coreutils}/bin/rm -rf "$TMP_DIR"
        ''
      |]
  liftIO $ readFile testOutputFilePath

buildNixDrvPath :: FilePath -> TestM FilePath
buildNixDrvPath drvPath = do
  liftIO $
    Process.readProcess
      "nix"
      [ "build",
        "--no-link",
        "--print-out-paths",
        drvPath <> "^*"
      ]
      ""

evalNixExpr :: (Aeson.FromJSON a) => String -> TestM a
evalNixExpr nixExpr = do
  tmpDir <- getTestDir
  liftIO $
    writeFile
      (tmpDir </> "test.nix")
      [i|
        let
          pkgs = import <nixpkgs> {};
          lib = pkgs.lib;
          sh = script: (pkgs.writeShellApplication { name = "test-script"; text = script; });
          jail-nix = import <jail-nix> {};
          jail = jail-nix.extend { inherit pkgs; };
        in #{nixExpr}
      |]
  liftIO $
    fromJust . Aeson.decode' . cs
      <$> Process.readProcess
        "nix"
        [ "eval",
          "--json",
          "--file",
          tmpDir </> "test.nix"
        ]
        ""

shouldOutput :: (HasCallStack) => String -> String -> TestM ()
shouldOutput nixDrvExpr expectedOutput = do
  out <- runNixDrv nixDrvExpr
  liftIO $ out `shouldBe` expectedOutput

shouldReturnTestM :: (HasCallStack, Show a, Eq a) => TestM a -> a -> TestM ()
shouldReturnTestM action expected = lifted' (`shouldReturn` expected) action

shouldEventuallyReturn :: (HasCallStack, Eq a, Show a) => TestM a -> a -> TestM ()
shouldEventuallyReturn action expected = go 0
  where
    delay = 20_000
    maxRetries = 5_000_000 `div` delay
    go :: Int -> TestM ()
    go retryCount = do
      result <- action
      if result == expected
        then pure ()
        else do
          if retryCount > maxRetries
            then liftIO $ result `shouldBe` expected
            else do
              liftIO $ threadDelay delay
              go $ retryCount + 1
