MAKE=gmake

all:
	cd interface; $(MAKE)
	cd intinf; $(MAKE)
	cd mgd; $(MAKE)
	cd mgdbm; $(MAKE)
	cd mmysql; $(MAKE)
	cd mpq; $(MAKE)
	cd mregex; $(MAKE)
	cd msocket; $(MAKE)
	cd munix; $(MAKE)

install:
	cd intinf; $(MAKE) install
	cd mgd; $(MAKE) install
	cd mgdbm; $(MAKE) install
	cd mmysql; $(MAKE) install
	cd mpq; $(MAKE) install
	cd mregex; $(MAKE) install
	cd msocket; $(MAKE) install
	cd munix; $(MAKE) install

uninstall:
	cd intinf; $(MAKE) uninstall
	cd mgd; $(MAKE) uninstall
	cd mgdbm; $(MAKE) uninstall
	cd mmysql; $(MAKE) uninstall
	cd mpq; $(MAKE) uninstall
	cd mregex; $(MAKE) uninstall
	cd msocket; $(MAKE) uninstall
	cd munix; $(MAKE) uninstall

test:
	cd interface; $(MAKE) test
	cd intinf; $(MAKE) test
	cd mgd; $(MAKE) test
	cd mgdbm; $(MAKE) test
	cd mmysql; $(MAKE) test
	cd mpq; $(MAKE) test
	cd mregex; $(MAKE) test
	cd munix; $(MAKE) test

clean:
	cd crypt; $(MAKE) clean
	cd interface; $(MAKE) clean
	cd intinf; $(MAKE) clean
	cd mgd; $(MAKE) clean
	cd mgdbm; $(MAKE) clean
	cd mmysql; $(MAKE) clean
	cd mpq; $(MAKE) clean
	cd mregex; $(MAKE) clean
	cd msocket; $(MAKE) clean
	cd munix; $(MAKE) clean
