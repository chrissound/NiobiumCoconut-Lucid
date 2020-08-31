#!/usr/bin/env bash
if which nix; then
    name=$(nix-shell -p 'python3.withPackages (ps: with ps; [ ps.pyyaml ])' --run "python -c 'import sys, yaml, json; print(json.dumps(yaml.safe_load(sys.stdin.read())))'" < package.yaml | jq -r '.name')

    nix-shell --run "ghcid --command='cabal v2-repl $name' --test=Main.main"
    exit 0
elif which stack; then
    ghcid '--command=stack ghci' --test='Main.main'
    exit 0
fi
