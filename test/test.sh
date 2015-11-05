#!/bin/sh

cd ../
make clean
cd test/
echo "Accept Tests:" >> test_results.txt
for filename in tree*accept.txt; do
        echo "$filename" >> test_results.txt
        menhir --interpret ../parser.mly < "$filename" | { 
        if grep -q "REJECT"; then echo "FAILURE: $filename" >> test_results.txt;
        else echo "SUCCESS: $filename" >> test_results.txt;
        fi
        }
done
echo "****************************" >> test_results.txt
echo "Reject Tests:" >> test_results.txt
for filename in tree*reject.txt; do
        echo "$filename" >> test_results.txt
        menhir --interpret ../parser.mly < "$filename" | { 
        if grep -q "ACCEPT"; then echo "FAILURE: $filename" >> test_results.txt;
        else echo "SUCCESS: $filename" >> test_results.txt;
        fi 
        }
done
