# Making and installing ML Server Pages version 1, for Moscow ML 2.00

# NB: Must set `MSPDIR' and `mosmlc' in mspcompile.sml prior to compilation.

MSPDIR=/var/lib/httpd/cgi-bin
MOSMLLIB=/usr/local/mosml/lib

SCRIPTDIR=${MSPDIR}/mscripts

all: 
	mosmlc -standalone -o mspcompile mspcompile.sml

install: 
	cp -p mspcompile ${MSPDIR}
	mkdir ${SCRIPTDIR}
	chown nobody ${SCRIPTDIR}

clean:
	rm -f *.ui *.uo
	rm -f mspcompile
