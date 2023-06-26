#!/bin/sh

testmi () {
  set +e
  binary=$(mktemp)
  compile_cmd="mi compile --test --disable-optimizations --output $binary"
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

testcppl () {
  set +e
  # TODO(dlunde,2023-06-26): Hardcoded, add support for temporary file in
  # cppl-mc.
  compile_cmd="cppl --seed 0"
  output=$1
  output="$output\n$($compile_cmd $1 2>&1)"
  exit_code=$?
  if [ $exit_code -ne 0 ]
  then
    echo "ERROR: command '$compile_cmd $1 2>&1' exited with $exit_code"
    exit 1
  fi
  echo "$output"
  set -e

  testmi "out.mc"

  rm "out.mc"
}

case $1 in
  test)
    testmi "$2"
    ;;
  test-cppl)
    testcppl "$2"
    ;;
  *)
    >&2 echo "Incorrect argument"
    exit 1
    ;;
esac
