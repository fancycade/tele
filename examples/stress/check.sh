#! /bin/sh

set -- \
  "stress.app" \
  "stress.beam" \
  "stress_math.beam" \
  "stress_protocol.beam" \
  "stress_statements.beam" \
  "stress_types.beam" \
  "stress_basic.beam" \
  "stress_hello_goodbye.beam" \
  "stress_real_world.beam" \
  "stress_test.beam"

for file in "$@"; do
  if ! [ -f "_build/default/lib/stress/ebin/$file" ]; then
    echo "$file not found"
    exit 1
  fi
done
