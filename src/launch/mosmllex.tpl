#!/bin/sh

stdlib=LIBDIR
mosmlbin=BINDIR

while : ; do
  case $1 in
    "")
      exec $mosmlbin/camlrunm $stdlib/mosmllex;;
    -*)
      echo "Unknown option \"$1\", ignored" >&2;;
    *)
      exec $mosmlbin/camlrunm $stdlib/mosmllex $* ;;
  esac
  shift
done


