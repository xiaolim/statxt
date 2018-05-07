#!/bin/bash
touch temp.stxt
cat examples/stdlib.stxt > temp.stxt
cat $1 >> temp.stxt
./statxt.sh temp.stxt
rm temp.stxt
