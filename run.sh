#/bin/bash
                                
# TODO: Specify the compiler invocation
#
# You *MUST* replace the following command with the
# command for invoking your compiler.

fileName=${1%.*}
./src/assign2-exe $fileName < "$1"

# For example, if you produce a flex/bison binary file
# "minic" in the src directory, you can invoke it using
# ./src/minic < "$1"
