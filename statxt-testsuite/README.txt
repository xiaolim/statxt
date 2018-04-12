Language: Statxt

Members:
Darpan Choudhary, System Architect
dc3292@columbia.edu 
Xiao Lim, Project Manager
xl2669@columbia.edu 
Zion Lin, Language Guru
zel2109@columbia.edu 
Katherine Patton, Tester
kyp2106@columbia.edu 


Section 1: Compilation
Just run the make file:
usage:
	make


Section 2: Execute compiler
Once the make command is executed, statxt.native is created and
we can pass arguments into the statxt.native executable
usage:
	./statxt.native < ./tests/test-hello.stxt
or:
	./statxt.native
To end, just use CTRL-D for EOF token. It will return 
what the scanner and parser could process, or throw an 
exception.



Section 3: Running testscript
From the same directory, just run the script
usage:
	./testall.sh
It will read the tests from the /tests/
directory included in the submission and compare the output
with expected output in the *.out file.

The test-hello.stxt program in /tests/ simply prints 3 integers.
The test-helloworld.stxt program in /tests/ simply prints the string "hello world"