#!/bin/sh
# Generate build_info_data.ml with git and timestamp metadata.
# Called by dune during build.
set -e
TARGET="$1"
commit=$(git rev-parse --short HEAD 2>/dev/null || echo unknown)
dirty=$(if git diff --quiet HEAD 2>/dev/null; then echo false; else echo true; fi)
build_time=$(date -u '+%Y-%m-%d %H:%M:%S UTC')

cat > "$TARGET" <<EOF
let git_commit = "$commit"
let git_dirty = $dirty
let build_time = "$build_time"
EOF
