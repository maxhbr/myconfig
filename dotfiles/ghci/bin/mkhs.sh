#!/usr/bin/env bash

initGit="true"
initCabal="true"
initTests="true"
useNix="true"
useGuards="true"
initSandboxIfNoNix="true"

while [[ $# > 0 ]]; do
    case $1 in
        -git) initGit="false" ;;
        +git) initGit="true" ;;
        -cabal) initCabal="false" ;;
        +cabal) initCabal="true" ;;
        -test|-tests) initTests="false" ;;
        +test|+tests) initTests="true" ;;
        -nix) useNix="false" ;;
        +nix) useNix="true" ;;
        -guard|-guards) useGuards="false" ;;
        +guard|+guards) useGuards="true" ;;
        -sandbox) initSandboxIfNoNix="false" ;;
        +sandbox) initSandboxIfNoNix="true" ;;
    esac
    shift
done

################################################################################
# global variables
have() { type "$1" &> /dev/null; }
DIR=$( pwd )
pkg=$( basename $DIR )
cblFile="${pkg}.cabal"

################################################################################
# git
# {{{
[[ -e .git ]] || {
    [[ "$initGit" = "true" ]] && git init
}
# }}}

################################################################################
# cabal
# {{{
[[ -e $cblFile ]] || {
    [[ "$initCabal" = "true" ]] && {
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
        [[ "$initTests" = ""true"" ]] && {
            cat >>$cblFile <<EOL


-- You can disable the spec test suite with -f-test-spec
flag test-spec
  default: True
  manual: True
-- You can disable the hlint test suite with -f-test-hlint
flag test-hlint
  default: True
  manual: True

test-suite spec
  type: exitcode-stdio-1.0
  hs-source-dirs: test

  main-is: Spec.hs

  if !flag(test-spec)
    buildable: False
  else
    build-depends: base  == 4.*
                 , hspec >= 1.3
                 , QuickCheck
  default-language:    Haskell2010

test-suite hlint
  type: exitcode-stdio-1.0
  main-is: hlint.hs
  ghc-options: -w -threaded -rtsopts -with-rtsopts=-N
  hs-source-dirs: ./
  default-language:    Haskell2010

  if !flag(test-hlint)
    buildable: False
  else
    build-depends: base
                 , hlint >= 1.7
EOL
        }
        git add $cblFile
        git add Setup.hs
        git add LICENSE
        [[ -e README.md ]] || touch add README.md
        git add README.md
    }
}
# }}}

mkdir -p src

################################################################################
# tests
# {{{
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

    [[ -f hlint.hs ]] || {
        cat >>hlint.hs <<EOL
-- stolen from https://github.com/ekmett/lens/blob/master/tests/hlint.hs
module Main where

import Control.Monad
import Language.Haskell.HLint
import System.Environment
import System.Exit

main :: IO ()
main = do
  args <- getArgs
  hints <- hlint $ ["src", "--cpp-define=HLINT", "--cpp-ansi"] ++ args
  unless (null hints) exitFailure
EOL
        git add hlint.hs
    }
}
# }}}

################################################################################
# nix and scripts
# {{{
[[ "$useNix" = "true" ]] && {
    have nix-env && {
        have cabal2nix || nix-env -iA nixpkgs.cabal2nix
        # uptare nix files
        cabal2nix ./ > default.nix
        cabal2nix --shell ./ > shell.nix
        git add *.nix

        [[ -f runCabal.sh ]] || {
            cat >>runCabal.sh <<EOL
#!/usr/bin/env bash
if [[ \$# -eq 0 ]] ; then
  nix-shell -I ~ --command "cabal repl"
else
  nix-shell -I ~ --command "cabal \$@"
fi
EOL
            chmod +x runCabal.sh
            git add runCabal.sh
        }
    }
} || {
    [[ "$initSandboxIfNoNix" = "true" ]] && {
        [[ -e .cabal-sandbox ]] || {
            cabal sandbox init
        }
    }
}
# }}}

################################################################################
# reconfigure
# {{{
[[ "$useNix" = "true" ]] && {
    # reconfigure
    nix-shell -I ~ --command 'cabal configure --enable-tests'
    # install all dependencies
    nix-shell -I ~ --command 'cabal install --only-dependencies --enable-tests'
} || {
    [[ -e .cabal-sandbox ]] && {
        cabal configure --enable-tests
        cabal install --only-dependencies --enable-tests
    }
}
# }}}

################################################################################
# guards
# {{{
have gem && [[ "$useGuards" = "true" ]] && {
    have guard || {
        gem install guard-shell
        gem install guard-haskell
    }
    [[ -f runGuard.sh ]] || {
        [[ "$useNix" = "true" ]] && {
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
        } || {
        cat >>runGuard.sh <<EOL
#!/usr/bin/env bash
if [[ -z "\$TMUX" ]]; then
  if tmux has-session -t "${pkg}-Guard" 2>/dev/null; then
    tmux att "${pkg}-Guard"
  else
    tmux new-session -s "${pkg}-Guard" "guard start"
  fi
else
  guard start
fi
EOL
        }
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
# }}}
