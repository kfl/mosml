#!/bin/sh

stdlib=LIBDIR
mosmlbin=BINDIR

linkalso=true
includes=""
compopt="-conservative"
linkopt=""
custom=""
linkfiles=""
cc=CC
ccfiles=""
cclib=""
ccopt=""
linkout=a.out
context="-structure"

while : ; do
  case $1 in
    "")
      break;;
    *.sml)
      $mosmlbin/camlrunm $stdlib/mosmlcmp -stdlib $stdlib $includes $compopt $context $1 || exit $?
      case $1 in
	    */*)
	    context="$context `dirname $1`/`basename $1 .sml`.ui"
	    ;;
	    *) context="$context `basename $1 .sml`.ui"
	    ;;
      esac
      linkfiles="$linkfiles $1";;
    *.sig)
      $mosmlbin/camlrunm $stdlib/mosmlcmp -stdlib $stdlib $includes $compopt $context $1 || exit $?
      case $1 in
	    */*)
	    context="$context `dirname $1`/`basename $1 .sig`.ui"
	    ;;
	    *) context="$context `basename $1 .sig`.ui"
	    ;;
      esac
      ;;
    *.ui)
      context="$context $1"
      ;;
    *.uo)
      linkfiles="$linkfiles $1";;
    -structure|-toplevel)
      context="$context $1";;
    -c)
      linkalso=false;;
    -I|-include)
      includes="$includes -I $2"
      shift;;
    -P|-perv)
      compopt="$compopt -P $2"
      linkopt="$linkopt -P $2"
      shift;;
    -q|-quotation)
      compopt="$compopt $1";;
    -i)
      compopt="$compopt $1"
      linkopt="$linkopt $1";;
    -g|-debug)
      compopt="$compopt $1"
      linkopt="$linkopt $1";;
    -m|-msgstyle)
      compopt="$compopt -msgstyle $2"
      shift;;
    -noheader)
      linkopt="$linkopt $1";;
    -noautolink)
      linkopt="$linkopt $1";;
    -o|-exec)
      linkout=$2
      shift;;
    -standalone)
      linkopt="$linkopt $1";;
    -stdlib)
      stdlib=$2
      shift;;
    -v|-version)
      echo "The Moscow ML system, version VERSION"
      echo "  (standard library from $stdlib)"
      $mosmlbin/camlrunm -V
      $mosmlbin/camlrunm $stdlib/mosmlcmp -version
      $mosmlbin/camlrunm $stdlib/mosmllnk -version;;
    -imptypes)
      compopt="$compopt $1";;
    -valuepoly)
      compopt="$compopt $1";;
    -orthodox|-conservative|-liberal)
      compopt="$compopt $1";;
    -files)
      linkfiles="$linkfiles $1 $2"
      shift;;
    -*)
      echo "Unknown option \"$1\", ignored" >&2;;
    *)
      echo "I don't know what to do with file \"$1\", ignored" >&2;;
  esac
  shift
done

if $linkalso && test -n "$linkfiles"; then
  $mosmlbin/camlrunm $stdlib/mosmllnk -stdlib $stdlib $includes $custom $linkopt \
    -exec $linkout $linkfiles || exit $?
fi

exit 0
