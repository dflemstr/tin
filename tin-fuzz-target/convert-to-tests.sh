#!/bin/bash
set -eu
cd "$(dirname "$0")/.."
mkdir -p testdata/ok/fuzz
for dir in /tmp/tin-afl-release/out/worker*
do for file in "$dir"/crashes/id:*
   do cp "$file" testdata/ok/fuzz/$(basename "$dir")-$(basename "$file").tn || true
   done
done
