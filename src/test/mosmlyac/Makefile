all: test1 test3

test1:
	mosmlyac test1.grm
	mosmlc -c test1.s??

test3: 
	mosmlyac test3.grm
	mosmllex test3lex.lex
	mosmlc -c test3aux.sml test3.s?? test3lex.s?? test3main.sml
	mosml test3load	

clean:
	rm -f *.ui *.uo 
	rm -f test1.s?? test2.s?? test3.s?? test3lex.s??
