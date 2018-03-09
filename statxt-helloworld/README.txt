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
Andrew Quijano, Tester
afq2101@columbia.edu 



Section 1: Compilation
Just run the make file:
usage:
	make



Section 2: Execute compiler
Once the make command is executed, statxt.native is created and
we can pass arguments into the statxt.native executable
usage:
	./statxt.native < ./cases/goodtest1.st
or:
	./statxt.native
To end, just use CTRL-D for EOF token. It will return 
what the scanner and parser could process, or throw an 
exception.



Section 3: Running testscript
From the same directory, just run the script
usage:
	./testscript 
It will read the 5 good and 5 bad cases from the /cases/
directory included in the submission.



Section 4: Additional Syntax
The only thing left that we are considering is supporting declaring variables
and assigning values to that declared variable in the same line.

What we did add:
string, char, structs, arrays, 
ability to mix variable declarations and assignments in functions
accessing elements in structs, accessing elements in arrays, 
assigning all elements of array at once


