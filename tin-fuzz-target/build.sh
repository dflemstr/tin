#!/bin/bash
set -eux
echo core | sudo tee /proc/sys/kernel/core_pattern 1>/dev/null
echo performance | sudo tee /sys/devices/system/cpu/cpu*/cpufreq/scaling_governor 1>/dev/null
cd "$(dirname "$0")"
export RUSTFLAGS='-Clink-arg=-fuse-ld=gold'
export RAYON_NUM_THREADS=1
cargo afl build --release

basedir=/tmp/tin-afl-release/
mkdir -p "$basedir"

n=$(nproc)
sqrtn=$(echo "sqrt($n)" | bc)
for i in $(seq $n)
do
  if [[ "$i" -eq 1 ]]
  then prefix="new-session -d -s tin-afl" flag=-M
  elif [[ $(( "$i" % "$sqrtn" )) -eq 0 ]]
  then prefix="split-window -v" flag=-S
  else prefix="split-window -h" flag=-S
  fi

  tmux $prefix "cargo afl fuzz -i ../testdata/ok -o $basedir/out $flag worker$i ../target/release/tin-fuzz-target"
  tmux select-layout tiled
done

tmux attach
