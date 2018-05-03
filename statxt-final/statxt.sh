#!/bin/bash

LLI="lli"

LLC="llc"

CC="cc"
STATXT="./statxt.native"

Run() {
    echo $* 1>&2
    eval $* || {
	SignalError "$1 failed on $*"
	return 1
    }
}

basename=`echo $1 | sed 's/.*\\///
                         s/.stxt//'`

Run "$STATXT" "$1" ">" "${basename}.ll" &&
Run "$LLC" "${basename}.ll" ">" "${basename}.s" &&
Run "$CC" "-o" "${basename}.exe" "${basename}.s" "printbig.o" &&
Run "./${basename}.exe"

