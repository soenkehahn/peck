ghcid:
  ghcid --command "stack ghci peck:test:spec" --test Main.main -W --color --reverse-errors --no-height-limit --poll

ci:
  stack clean
  stack test --ghc-options=-Werror
