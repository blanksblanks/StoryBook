#!/bin/sh

cd ../
make clean
make

cd test
for sbkname in *.sbk;do
        program=`basename $sbkname .sbk`
        .././run < "$sbkname" > "${program}.c"
        if [ -s "$program.c" ]
        then
          gcc -g $program.c -o $program
          if [ -f "$program" ]
          then
            ./$program
            ./$program > "${program}Out.txt"
            diff "${program}Out.txt" "${program}Exp.txt"
          else 
            echo "      /\_/\\ !! _
            =( °∩° )= //
              )   (  //
             (__ __)//"          
          fi
        else
        echo "          /\_/\\ !! _
        =( °∩° )= //
          )   (  //
         (__ __)//"
        fi
done
