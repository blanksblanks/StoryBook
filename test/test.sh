#!/bin/sh

cd ../
make clean
cd test/tree
echo "Accept Tests:" >> test_results.txt
for filename in tree*accept.txt; do
        menhir --interpret ../../parser.mly < "$filename" | { 
        if grep -q "ACCEPT"; then echo "SUCCESS: $filename" >> test_results.txt;
        else echo "FAILURE: $filename" >> test_results.txt;
        fi
        }
done
echo "****************************" >> test_results.txt
echo "Reject Tests:" >> test_results.txt
for filename in tree*reject.txt; do
        menhir --interpret ../../parser.mly < "$filename" | { 
        if grep -q "REJECT"; then echo "SUCCESS: $filename" >> test_results.txt;
        else echo "FAILURE: $filename" >> test_results.txt;
        fi 
        }
done
