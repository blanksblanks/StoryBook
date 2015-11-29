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
            ./$program > "${program}Out.txt"
            diff "${program}Out.txt" "${program}_Exp.txt"
            rm $program
            echo "SUCCESS: $program" >> test_results.txt;
          else
            echo "      /\_/\\ !! _
            =( °∩° )= //
              )   (  //
             (__ __)//"  
             echo "FAILURE: $program" >> test_results.txt; 
             echo "FAILURE: $program"       
          fi
        else
        echo "          /\_/\\ !! _
        =( °∩° )= //
          )   (  //
         (__ __)//"
        echo "FAILURE: $program" >> test_results.txt; 
        echo "FAILURE: $program"   
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
          echo "FAILURE: $program" >> test_results.txt
          echo "FAILURE: $program"
        echo "          /\_/\\ !! _
        =( °∩° )= //
          )   (  //
         (__ __)//"
        fi
done

rm -rf *.dSYM
