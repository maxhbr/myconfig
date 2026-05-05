module SeccompSpec (spec) where

import System.Info (arch)
import TestPrelude
import Test.Hspec

spec :: Spec
spec = parallel $ inTestM . before_ checkArch $ do
  describe "add-seccomp" $ do
    it "blocks syscalls with the given filter" $ do
      nixExprWithGenBpf [i|
        let
          bpf = simpleBpf "SCMP_ACT_ALLOW" ''
            seccomp_rule_add(ctx, SCMP_ACT_KILL, SCMP_SYS(mkdir), 0);
          '';
        in
          jail "test" (sh "{ mkdir foo ; } &>/dev/null || echo $?") (c: [
            (c.add-seccomp bpf)
          ])
      |]
        `shouldOutput` "159\n"

    it "works with bpf producing executables" $ do
      nixExprWithGenBpf [i|
        let
          bpf = simpleBpf "SCMP_ACT_ALLOW" ''
            seccomp_rule_add(ctx, SCMP_ACT_KILL, SCMP_SYS(mkdir), 0);
          '';
        in
          jail "test" (sh "{ mkdir foo ; } &>/dev/null || echo $?") (c: [
            (c.add-seccomp (lib.getExe (sh "cat ${bpf}")))
          ])
      |]
        `shouldOutput` "159\n"

    it "composes multiple filters" $ do
      out <- runNixDrv $ nixExprWithGenBpf [i|
        let
          bpfA = simpleBpf "SCMP_ACT_ALLOW" ''
            seccomp_rule_add(ctx, SCMP_ACT_KILL, SCMP_SYS(mkdir), 1,
                             SCMP_CMP(1, SCMP_CMP_EQ, 0755));
          '';
          bpfB = simpleBpf "SCMP_ACT_ALLOW" ''
            seccomp_rule_add(ctx, SCMP_ACT_ERRNO(1000), SCMP_SYS(mkdir), 0);
          '';
          bpfC = simpleBpf "SCMP_ACT_ALLOW" ''
            seccomp_rule_add(ctx, SCMP_ACT_ERRNO(1001), SCMP_SYS(mkdir), 0);
          '';
          shj' = script: (sh script).overrideAttrs (_: {
            runtimeInputs = with pkgs; [ strace gnugrep ];
            bashOptions = [];
          });
        in
          jail "test" (sh ''
            {
              mkdir -m777 foo 2>&1; echo $?
              mkdir -m755 foo 2>&1; echo $?
            } 2>&1 || true
          '') (c: [
            (c.add-seccomp bpfA)
            (c.add-seccomp bpfB)
            (c.add-seccomp bpfC)
          ])
      |]
      -- Last filter defined is prioritized (bpfC), unless a rule in a preceding
      -- filter matches and has a "stricter" action (bpfA for 755 mkdir args).
      liftIO $ case lines out of
        [err1, status1, err2, status2] -> do
          err1 `shouldEndWith` "1001"
          status1 `shouldBe` "1"
          err2 `shouldContain` "Bad system call"
          status2 `shouldBe` "159"
        _ -> expectationFailure "failed to pattern match on output"

nixExprWithGenBpf :: String -> String
nixExprWithGenBpf expr =
  [i|
    let
      generateBpf = code: pkgs.runCommandCC "bpf-bytecode" {
        inherit code;
        passAsFile = [ "code" ];
        buildInputs = [ pkgs.libseccomp ];
      } ''
        $CC -x c "$codePath" -lseccomp -o generator
        ./generator > $out
      '';
      simpleBpf = defaultAction: code: generateBpf ''
        #include <seccomp.h>
        int main(void) {
          scmp_filter_ctx ctx = seccomp_init(${defaultAction});
          ${code}
          seccomp_export_bpf(ctx, 1);
        }
      '';
    in
      #{expr}
  |]

checkArch :: IO ()
checkArch = case arch of
  "x86_64" -> return ()
  _        -> pendingWith $ "test not validated for arch " ++ arch
