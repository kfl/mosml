# Makefile for mosmllib test cases

# with the supplied compiler and the supplied library
all:
	mosml -P full test.sml > result 2>&1
	diff result result.ok

# with the supplied compiler and the current library
libtest:
	mosml -I .. -P full test.sml > result 2>&1
	diff result result.ok

# with the current compiler and the current library 
newtest:
	../../camlrunm ../../compiler/mosmltop -stdlib .. -P full test.sml > result 2>&1
	diff result result.ok

# with the current compiler and the current library (as above)
current: 
	make newtest

cmdline: 
	mosmlc -o cmdline cmdline.sml 
	cmdline arg1 arg2 arg3

clean:
	rm -f result 
	rm -f cmdline cmdline.ui cmdline.uo
	rm -f empty.dat medium.dat  small1.dat  small2.dat  text.dat 
	rm -f mosmltestrun
	cd callback; make clean
