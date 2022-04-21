#!/bin/sh

test () {
  set +e
  binary=$(mktemp)
  compile_cmd="mi compile --test --disable-optimizations --typecheck --output $binary"
  output=$1
  output="$output\n$($compile_cmd $1 2>&1)"
  exit_code=$?
  if [ $exit_code -eq 0 ]
  then
    output="$output$($binary)"
    exit_code=$?
    if [ $exit_code -eq 0 ]
    then
      rm $binary
    else
      echo "ERROR: compiled binary for $1 exited with $exit_code"
      rm $binary
      exit 1
    fi
  else
    echo "ERROR: command '$compile_cmd $1 2>&1' exited with $exit_code"
    exit 1
  fi
  echo "$output\n"
  set -e
}

case $1 in
  test)
    test "$2"
    ;;
  *)
    >&2 echo "Incorrect argument"
    exit 1
    ;;
esac
