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

Welcome to Statxt. This is the result of a long semester's work on our langage Statxt and its compiler. 

Statxt is a language which specializes in analyzing textual statistics. 


This file contains a lot of code which consists of three groups: 
1. The compiler code
2. The compiler feature tests (QA for our features) 
3. The demo code



Section 1: 
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
testall.sh (tests all)
statxt.sh  (tests on specified program)

The rest of the function testing can be found in /tests 


Section 3: 
The demo code consists of a shell script on the top level that automates its testing:
runexample.sh
And the rest of the demo code can be found in /examples 

the three tests are:
syllables.stxt 
word_frequency.stxt
book_record.stxt 


More about the demo code: 
syllables.stxt 




		