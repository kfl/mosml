#!/bin/sh

stdlib=LIBDIR
mosmlbin=BINDIR
includes=""
options="-conservative"

while : ; do
  case $1 in
    "")
      exec $mosmlbin/camlrunm $stdlib/mosmltop -stdlib $stdlib $includes $options;;
    -I|-include)
      includes="$includes -I $2"
      shift;;
    -P|-perv)
      options="$options -P $2"
      shift;;
    -imptypes)
      options="$options -imptypes"
      ;;
    -m|-msgstyle)
      options="$options -msgstyle $2"
      shift;;
    -quietdec)
      options="$options -quietdec"
      ;;
    -valuepoly)
      options="$options -valuepoly"
      ;;
    -orthodox|-conservative|-liberal)
      options="$options $1"
      ;;
    -stdlib)
      stdlib=$2
      shift;;
    -*)
      echo "Unknown option \"$1\", ignored" >&2;;
    *)
      exec $mosmlbin/camlrunm $stdlib/mosmltop -stdlib $stdlib $includes $options $* ;;
  esac
  shift
done


