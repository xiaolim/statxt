Language: Statxt

Members:
Darpan Chaudhary, System Architect
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
with expected output in the *.out file or *.err file depending
on whether the test is expected to run correctly or produce an error.



Tests added for the test suite deliverable:
"Bad":
	fail-access-nonexistent-member.stxt
		fails because it tries to access a member (a) of p, but p is a struct point which does not include a member called a.

	fail-access-nonexistent-struct.stxt
		fails because it tries to assign 1 to p1.x even though a struct called p1 has not been declared.

	fail-assign-int-float.stxt
		fails because it tries to assign p1.f (a float) to p1.i (an int).

"Good":
	test-hello-from-str.stxt
		declares a string x, then assigns "Hello World" to x, and prints x. tests feature to print from a string variable.

	test-struct-decl.stxt
		declares a struct point with 3 int members, sets each member to 0, then prints p1.x (the first member).  tests feature to print int member of struct.

	test-struct-print-float.stxt
		declares a char, and int, and assigns to them, then declares a struct tests s1 with 2 int members and a float member, assigns values to the struct members and prints a float from the struct s1.  tests feature to print float member of struct.

	test-struct-return.stxt
		declares a struct book b1 and a function that creatse a struct book, assigns the title member of that struct and returns it, then calls the function to assign the functions return type to a struct book b1, then prints the title member of struct book b1.  tests feature to return a struct type from a function.

	test-structs.stxt
		declares structs point and pointf, then tests assigning correct values of correct types (ints and floats) to the members of the tructs and prints them to verify.

	test-structs2.stxt
		declares structs Book and Chapter and assigns values of the correct types (ints and strings) to them and prints them out to verify.

	test-structs-load-assign.stxt
		declares structs types and types2 with a bunch of different types (int, char, float, string) of members, assigns values to each of them in t1, then assigns values in t2 from t1 and prints t2 to verify.
		