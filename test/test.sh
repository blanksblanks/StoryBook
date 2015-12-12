#!/bin/sh

cd ../
make clean
make

cd test
echo "Accept Tests:" >> test_results.txt
failcount=0
passcount=0
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
            rm $program
            if  diff -q "${program}_Out.txt" "${program}_Exp.txt"
            then
              let "passcount += 1"
              echo "✅ : $program" >> test_results.txt;
            else
              let "failcount += 1"
              echo "❌ : $program -- Compiled and ran, but wrong output." >> test_results.txt
              echo "❌ : $program -- Compiled and ran, but wrong output."
            fi
          else
            echo "🙈"
             let "failcount += 1"
             echo "❌ : $program -- C Code wouldn't compile" >> test_results.txt;
             echo "❌ : $program"
          fi
        else
        echo "🙈"
        let "failcount += 1"
        echo "❌ : $program -- Storybook didn't compile" >> test_results.txt;
        echo "❌ : $program -- Storybook didn't compile"
        fi
done

for rejectname in *_Reject.sbk;do
        program=`basename $rejectname _Reject.sbk`
        echo "Test: $program" >> errors.txt
        .././run < "$rejectname" > "${program}.c" 2>> errors.txt
        if [ ! -s "$program.c" ]
        then
          let "passcount += 1"
          echo "✅ : $program" >> test_results.txt

        else
          let "failcount += 1"
          echo "❌ : $program -- Storybook compiled but should not have" >> test_results.txt
          echo "❌ : $program -- Storybook compiled but should not have"
        echo "🙈"
        fi
done
echo "$passcount tests passed"
echo "$failcount tests failed"
rm -rf *.dSYM
