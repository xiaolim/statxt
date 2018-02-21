Section 1: Compilation
It will follow the exact same instructions as with Micro-C. 
Our code is being run on an Ubuntu 16.04 VM

sudo apt install ocaml llvm llvm-runtime m4 opam
opam init
opam install llvm.3.8
eval 'opam config env'
make

Section 2: Execute compiler
Once the make command was executed we can 

Section 3: Running testscript

Section 4: Additional Syntax
Currently, there is no further syntax we want to add.
However, we still have no implemented to be able to assign
values to struct elements. Also, we have no yet implemented arrays.
