# Makefile for mosmllib test cases

# with the supplied compiler and the supplied library
all: links
	mosml -P full test.sml > result 2>&1

diff:
	diff result result.ok

# with the supplied compiler and the current library
libtest: links
	mosml -I .. -P full test.sml > result 2>&1
	diff result result.ok

# with the current compiler and the current library 
newtest: links
	../../camlrunm ../../compiler/mosmltop -stdlib .. -P full test.sml > result 2>&1
	diff result result.ok

# with the current compiler and the current library (as above)
current: links
	make newtest

cmdline: 
	mosmlc -o cmdline cmdline.sml 
	cmdline arg1 arg2 arg3

# set up hardlinks and softlinks for filesys.sml
links:
	touch hardlinkA
	ln -f hardlinkA hardlinkB
	ln -sf README testlink
	ln -sf exists.not testbadl
	ln -sf testcycl testcycl

clean:
	rm -f result 
	rm -f cmdline cmdline.ui cmdline.uo
	rm -f empty.dat medium.dat  small1.dat  small2.dat  text.dat 
	rm -f mosmltestrun
	rm -f hardlinkA hardlinkB
	rm -f testlink testcycl testbadl
	rm -f testrun.ui testrun.uo
	cd callback; make clean
