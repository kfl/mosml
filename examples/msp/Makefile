# Making and installing ML Server Pages version 1.2, for Moscow ML 2.10

# NB: Must set `SCRIPTCACHE' and `mosmlc' in mspcompile.sml prior to compilation.

CGI_DIR=/usr/lib/cgi-bin
SCRIPTCACHE=/var/cache/mspscripts

all: 
	mosmlc -standalone -o mspcompile mspcompile.sml

install: 
	cp mspcompile ${CGI_DIR}
	mkdir ${SCRIPTCACHE}
	chown www-data:www-data ${SCRIPTCACHE}

clean:
	rm -f *.ui *.uo
	rm -f mspcompile
