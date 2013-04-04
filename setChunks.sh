#!/bin/bash

function findin {
  find . -name "$1" -exec grep -H "$2" \{\} \; ;
}

sed -i -r "s/^cHUNKS_PER_THREAD = .+/cHUNKS_PER_THREAD = $1/g;" Data/Array/Repa/Eval/Chunked.hs
sed -i -r "s/^cHUNKS_PER_THREAD = .+/cHUNKS_PER_THREAD = $1/g;" Data/Array/Repa/Eval/Selection.hs
findin "*.hs" "cHUNKS_PER_THREAD"

