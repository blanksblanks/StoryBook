#!/bin/sh

# Runnign the
./run < $1
gcc -g helloWorld.c -o helloWorld
./helloWorld
