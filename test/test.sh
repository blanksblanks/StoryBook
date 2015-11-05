#!/bin/sh

cd ../
make clean
cd test/
for filename in tree*.txt; do
	menhir --interpret --interpret-show-cst ../parser.mly < "$filename" > "output_$filename"
        echo "$filename" >> test_results.txt
        diff "exp_$filename" "output_$filename" >> test_results.txt
done
