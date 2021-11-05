#!/bin/sh

test () {
  set +e
  binary=tmp_test
  compile_cmd="mi compile --test --disable-optimizations --output $binary"
  output=$1
  output="$output\n$($compile_cmd $1 2>&1)"
  exit_code=$?
  if [ $exit_code -eq 0 ]
  then
    output="$output$(./$binary)"
    exit_code=$?
    if [ $exit_code -eq 0 ]
    then
      rm $binary
    else
      echo "ERROR: command ./$binary exited with $exit_code"
      rm $binary
    fi
  else
    echo "ERROR: command '$compile_cmd $1 2>&1' exited with $exit_code"
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
