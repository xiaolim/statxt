Language: Statxt
Members:




Section 1: Compilation
Just run the make file:

make

Section 2: Execute compiler
Once the make command was executed 
we can pass arguments into the statxt.native executable

Example:
./statxt.native < ./cases/goodtest1.st

To end, just use CTRL-D for EOF token. It will return 
what the scanner and parser could process, or throw an 
exception.

Section 3: Running testscript
From the same directory, just run the script

./testScript 

It will read the 5 good and 5 bad cases from the cases
directory included in the submission.

Section 4: Additional Syntax
Currently, there is no further syntax we want to add.

What we did add:
string, char
structs, arrays, ability to mix variable declarations and assignments in functions
accessing elements in structs, accessing elements in arrays, assigning all elements of array at once


