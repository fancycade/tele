#! /bin/sh

set -- \
   "hello_tele.app" \
   "hello_tele.beam"

for file in "$@"; do
  if ! [ -f "_build/default/lib/hello_tele/ebin/$file" ]; then
    echo "$file not found"
    exit 1
  fi
done
