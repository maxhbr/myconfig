#!/usr/bin/env bash

initGit="true"
initCabal="true"
initTests="true"
useCabal="true"
useGuards="true"

################################################################################
# global variables
have() { type "$1" &> /dev/null; }
DIR=$( pwd )
pkg=$( basename $DIR )
cblFile="${pkg}.cabal"

################################################################################
# git
[[ -e .git ]] || [[ "$initGit" = true ]] && git init

################################################################################
# cabal
[[ -e $cblFile ]] || [[ "$initCabal" = true ]] && {
    cabal init --minimal --no-comments \
          --package-name="$pkg" \
          --version=0.1.0.0 \
          --author=maximilianhuber \
          --license=BSD3 \
          --email=mail@maximilian-huber.de \
          --homepage= \
          --language=Haskell2010 \
          --source-dir=src \
          --main-is=Main.hs
    [[ "$initTests" = "true" ]] && {
        cat >>$cblFile <<EOL


test-suite spec
  type: exitcode-stdio-1.0
  hs-source-dirs: test

  main-is: Spec.hs

  build-depends: base  == 4.*
               , hspec >= 1.3
  default-language:    Haskell2010
EOL
    }
    git add $cblFile
    git add Setup.hs
    git add LICENSE
}

mkdir -p src

################################################################################
# tests
[[ "$initTests" = "true" ]] && {
    mkdir -p test
    [[ -f test/Spec.hs ]] || {
        cat >>test/Spec.hs <<EOL
{-# OPTIONS_GHC -F -pgmF hspec-discover #-}
module Name.Of.ModuleSpec where
{-
import SpecHelper
import Name.Of.Module

spec :: Spec
spec = do
       ...

main :: IO ()
main = hspec $ spec
-}
EOL
        git add test/Spec.hs
    }

    [[ -f test/SpecHelper.hs ]] || {
        cat >>test/SpecHelper.hs <<EOL
module SpecHelper
       ( module X
       ) where

import Test.Hspec as X
import Test.QuickCheck as X
import Control.Exception as X
EOL
        git add test/SpecHelper.hs
    }
}

################################################################################
# nix and scripts
have nix-env && [[ "$useCabal" = "true" ]] && {
    have cabal2nix || nix-env -iA nixpkgs.cabal2nix
    # uptare nix files
    cabal2nix ./ > default.nix
    cabal2nix --shell ./ > shell.nix
    git add *.nix
    # reconfigure
    nix-shell -I ~ --command 'cabal configure --enable-tests'
    # install all dependencies
    nix-shell -I ~ --command 'cabal install --only-dependencies --enable-tests'

    [[ -f runCabal.sh ]] || {
        cat >>runCabal.sh <<EOL
#!/usr/bin/env bash
if [[ $# -eq 0 ]] ; then
  nix-shell -I ~ --command "cabal repl"
else
  nix-shell -I ~ --command "cabal \$@"
fi
EOL
        chmod +x runCabal.sh
        git add runCabal.sh
    }

    have gem && [[ "$useGuards" = "true" ]] && {
        have guard || {
            gem install guard-shell
            gem install guard-haskell
        }
        [[ -f runGuard.sh ]] || {
            cat >>runGuard.sh <<EOL
#!/usr/bin/env bash
if [[ -z "\$TMUX" ]]; then
  if tmux has-session -t "${pkg}-Guard" 2>/dev/null; then
    tmux att "${pkg}-Guard"
  else
    tmux new-session -s "${pkg}-Guard" "nix-shell -I ~ --command 'guard start'"
  fi
else
  nix-shell -I ~ --command 'guard start'
fi
EOL
            chmod +x runGuard.sh
            git add runGuard.sh
        }

        [[ -f Guardfile ]] || {
            cat >>Guardfile <<EOL
# Runs the command and prints a notification
def execute(cmd)
  if system(cmd)
    n 'Build succeeded', 'hspec', :success
  else
    n 'Build failed', 'hspec', :failed
  end
end

def run_all_tests
  execute %{
    cabal configure --enable-tests &&
    cabal build && cabal test
  }
end

def run_tests(mod)
  specfile = "test/#{mod}Spec.hs"

  if File.exists?(specfile)
    files = [specfile]
  else
    files = Dir['test/**/*.hs']
  end

  execute "ghc -isrc -itest -e main #{files.join(' ')}"
end

guard :shell do
  watch(%r{.*\.cabal$})          { run_all_tests }
  watch(%r{test/SpecHelper.hs$}) { run_all_tests }
  watch(%r{src/(.+)\.hs$})       { |m| run_tests(m[1]) }
  watch(%r{test/(.+)Spec\.hs$})  { |m| run_tests(m[1]) }
end
EOL
            git add Guardfile
        }
    }
}
