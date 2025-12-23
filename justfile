format:
    fd -e hs | xargs -n 1 -P $(nproc) fourmolu -i -q

cabal2nix:
    #! /usr/bin/env bash
    for file in $(fd -e cabal)
    do
        dir="$(dirname $file)"
        cd "$dir"
        cabal2nix . > default.nix
        cd ..
        echo "Created $dir/default.nix"
    done
