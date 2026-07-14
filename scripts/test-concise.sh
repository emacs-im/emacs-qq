#!/usr/bin/env bash

set -u

compile_log="$(mktemp -t emacs-qq-compile.XXXXXX)"
test_log="$(mktemp -t emacs-qq-test.XXXXXX)"
trap 'rm -f "$compile_log" "$test_log"' EXIT

if ! eask recompile >"$compile_log" 2>&1; then
  echo "emacs-qq: byte compilation failed" >&2
  tail -n 200 "$compile_log" >&2
  exit 1
fi

if (( $# > 0 )); then
  eask test ert "$@" >"$test_log" 2>&1
  status=$?
else
  eask test ert test/*.el >"$test_log" 2>&1
  status=$?
fi

if (( status == 0 )); then
  grep -E '^(Running |Ran )' "$test_log"
else
  echo "emacs-qq: ERT failed (last 240 lines)" >&2
  tail -n 240 "$test_log" >&2
fi

exit "$status"
