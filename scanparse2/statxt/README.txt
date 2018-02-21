Section 1: Compilation
It will follow the exact same instructions as with Micro-C. 
Our code is being run on an Ubuntu 16.04 VM

sudo apt install ocaml llvm llvm-runtime m4 opam
opam init
opam install llvm.3.8
eval 'opam config env'
make

Section 2: Execute compiler
Once the make command was executed 
we can pass arguments into the statxt.native executable

Example:
./statxt.native < 5 + 5

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
However, we still have not been able to assign
values to struct elements. Also, we have no yet implemented arrays
beyond just declaring them.
