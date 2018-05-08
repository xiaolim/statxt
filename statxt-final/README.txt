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

Welcome to Statxt. This is the result of a long semester's work on our langage
Statxt and its compiler. 

Statxt is a language which specializes in analyzing textual statistics. 


This file contains a lot of code which consists of three groups: 
1. The compiler code
2. The compiler feature tests (QA for our features) 
3. The demo code



Section 1:
All files necessary for compilation are included in the root of the Statxt
folder.  Statxt can be compiled by simply running "make":
useage:
	make

Compilation of mystatxtcode.stxt can be achieve via the following steps or through the
process descriped in Section 2 or 3 below with the provided shell scripts:
usage:
	./statxt.native mystatxtcode.stxt > mystatxtcode.ll
	llc mystatxtcode.ll > mystatxtcode.s
	cc -o mystatxtcode.exe mystatxtcode.s cfunctions.o
	./mystatxtcode.exe

Files included: 
scanner.mll
parser.mly
ast.ml
semant.ml
sast.ml
codegen.ml
cfunctions.c
statxt.ml
stdlib.stxt
Makefile 

Section 2: 
Includes two shell scripts on the top level for automation of testing:
testall.sh (tests all (omits stdlib functions))
stdlibtestall.sh (tests all (copies in stdlib functions))
statxt.sh  (copies stdlib functions into given file and runs it)
usage:
	./testall.sh
	./stdlibtestall.sh
	./statxt.sh ./tests/test-structs.stxt

testall.sh runs all tests in tests/
stdlibtestall.sh runs all tests in stdlibtests/


Section 3: 
The demo code consists of a shell script on the top level that 
automates copying of the standard library functions and the contents of the
file passed in as an argument into a third temp file for compilation. the script
assumes that stdlib.stxt in the same directory:
statxt.sh
usage:
	./statxt.sh ./examples/syllables.stxt

The rest of the demo code can be found in /examples

The three tests are:
syllables.stxt 
word_frequency.stxt
book_record.stxt 


More about the demo code: 
syllables.stxt
This file print an estimate of the number of syllables of each word in the file,
the average number of syllables, and computes the Flesch Reading Ease Score as
well as the Flesch Kincaid grade level based on the sample text in
./examples/sample_test.txt

word_frequency.stxt
It prints the word frequency of each word in the sample text at
./examples/sample_text.txt

book_record.stxt
It stores grouped information about each book in a struct
and prints this information. 




		