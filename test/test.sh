#!/bin/sh

cd ../
make clean
make

cd test
echo "Accept Tests:" >> test_results.txt
for acceptname in *_Accept.sbk;do
        program=`basename $acceptname _Accept.sbk`
        echo "Test: $program" >> errors.txt
        .././run < "$acceptname" > "${program}.c" 2>> errors.txt
        if [ -s "$program.c" ]
        then
          gcc -g $program.c -o $program
          if [ -f "$program" ]
          then
            ./$program > "${program}_Out.txt"
            if  diff -q "${program}_Out.txt" "${program}_Exp.txt" 
            then
              rm $program
              rm "$program.c"
              echo "SUCCESS: $program" >> test_results.txt;
            else
              echo "FAILURE: $program -- Compiled and ran, but wrong output." >> test_results.txt
              echo "FAILURE: $program -- Compiled and ran, but wrong output."
            fi
          else
            echo "      /\_/\\ !! _
            =( °∩° )= //
              )   (  //
             (__ __)//"  
             echo "FAILURE: $program -- C Code wouldn't compile" >> test_results.txt; 
             echo "FAILURE: $program"       
          fi
        else
        echo "          /\_/\\ !! _
        =( °∩° )= //
          )   (  //
         (__ __)//"
        echo "FAILURE: $program -- Storybook didn't compile" >> test_results.txt; 
        echo "FAILURE: $program -- Storybook didn't compile"   
        fi
done

for rejectname in *_Reject.sbk;do
        program=`basename $rejectname _Reject.sbk`
        echo "Test: $program" >> errors.txt
        .././run < "$rejectname" > "${program}.c" 2>> errors.txt
        if [ ! -s "$program.c" ]
        then
          echo "SUCCESS: $program" >> test_results.txt
          rm "$program.c"
          
        else
          echo "FAILURE: $program -- Storybook compiled but should not have" >> test_results.txt
          echo "FAILURE: $program -- Storybook compiled but should not have"
        echo "          /\_/\\ !! _
        =( °∩° )= //
          )   (  //
         (__ __)//"
        fi
done

rm -rf *.dSYM
