#!/bin/sh

testmi () {
  set +e
  binary=$(mktemp)
  compile_cmd="mi compile --test --disable-optimizations --output $binary"
  [ -z "$output" ] && output="$1 "
  compile_output=$($compile_cmd $1 2>&1)
  exit_code=$?
  [ -n "$compile_output" ] && output="$output\n$compile_output"
  if [ $exit_code -eq 0 ]
  then
    output="$output$($binary 2>&1)"
    exit_code=$?
    echo "$output"
    if [ $exit_code -eq 0 ]
    then
      rm $binary
    else
      echo "ERROR: compiled binary for $1 exited with $exit_code"
      rm $binary
      exit 1
    fi
  else
    echo "$output"
    echo "ERROR: command '$compile_cmd $1 2>&1' exited with $exit_code"
    exit 1
  fi
  set -e
}

testcppl () {
  set +e
  compile_cmd="cppl --seed 0"
  output=$1
  compile_output=$($compile_cmd $1 2>&1)
  exit_code=$?
  [ -n "$compile_output" ] && output="$output\n$compile_output"
  if [ $exit_code -ne 0 ]
  then
    echo "$output"
    echo "ERROR: command '$compile_cmd $1 2>&1' exited with $exit_code"
    exit 1
  fi
  set -e

  output="$output "

  # TODO(dlunde,2023-06-26): "out.mc" hardcoded, add support for outputting to
  # temporary file in cppl-mc.
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
