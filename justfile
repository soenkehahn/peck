ghcid:
  #!/usr/bin/env bash

  export HSPEC_FAIL_FAST=yes
  export HSPEC_RERUN=yes
  export HSPEC_RERUN_ALL_ON_SUCCESS=yes

  ghcid --command "stack ghci peck:test:spec" --test Main.main -W --color --reverse-errors --no-height-limit --poll

ci:
  stack clean
  stack test --ghc-options=-Werror
