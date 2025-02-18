#!/bin/sh

testexec () {
  set +e
  output="$output$($1 2>&1)"
  exit_code=$?
  echo "$output"
  if [ $exit_code -eq 0 ]
  then
    rm $1
  else
    echo "ERROR: compiled binary $1 exited with $exit_code"
    rm $1
    exit 1
  fi
  set -e
}

testmi () {
  set +e
  binary=$(mktemp)
  compile_cmd="mi compile --test --disable-optimizations --output $binary"
  output="$1 "
  compile_output=$($compile_cmd $1 2>&1)
  exit_code=$?
  [ -n "$compile_output" ] && output="$output\n$compile_output"
  if [ $exit_code -eq 0 ]
  then
    testexec "$binary"
  else
    echo "$output"
    echo "ERROR: command '$compile_cmd $1 2>&1' exited with $exit_code"
    exit 1
  fi
  set -e
}

testcppl () {
  set +e
  cpplout=$(mktemp)
  compile_cmd="$2 --seed 0 --test --output $cpplout"
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

  testexec "$cpplout"
}

case $1 in
  test)
    testmi "$2"
    ;;
  test-cppl)
    testcppl "$2" "$3"
    ;;
  *)
    >&2 echo "Incorrect argument"
    exit 1
    ;;
esac
