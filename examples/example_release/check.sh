#! /bin/sh

set -- \
   "example_release.app" \
   "example_release_sup.beam" \
   "example_release_app.beam"

for file in "$@"; do
  if ! [ -f "_build/default/lib/example_release/ebin/$file" ]; then
    echo "$file not found"
    exit 1
  fi
done
