#!/bin/bash
#
# Kompile (if necessary) and Krun all test files for (all) specifications.
#
# If an input file is found (e.g., program.cink.in for program.cink), it is 
# fed to stdin. Otherwise if the program expects user input, it will wait until
# it gets one.
#
# Basic usage:
#   $ ./run-all.sh
#
# Run all tests for specific languages:
#   $ ./run-all.sh expressions threads
#
# All "interactive" test programs work with numbers so it is possible to have
# an unattended run with e.g.:
#   $ yes "10" | ./run-all.sh
#
# The run produces many log files in the language subdirectories. They can all 
# be deleted using:
#   $ find -name '*.run-all.log' | xargs rm
#
# The program 'tee' is useful to have a full log of the run while watching the
# output on the terminal at the same time:
#   $ ./run-all.sh | tee full.log
#
# 
# The script performs two simple tests:
#   - the 'skip test' checks whether the <k> cell only contains 'skip' at the
#     end of a krun; this means that the program didn't get stuck
#   - the 'thread test' checks whether the <threads> cell contains an empty bag;
#     this means that all threads have finished
#
# The 'skip test' is applicable to "statements" and "functions". The 'thread 
# test' is applicable to "threads", "references", and "pointers". Test programs
# for "expressions" and "declarations" have no such checks at the moment.
#
# A summary is printed at the end.

orig=languages
langs="expressions declarations statements functions threads references pointers"
top=`pwd`
tests="tests/*.cin[fk] programs/*.cin[fk] programs/tests/*.cin[fk]"

declare -A checks
checks["statements"]="skip"
checks["functions"]="skip"
checks["threads"]="thread"
checks["references"]="thread"
checks["pointers"]="thread"


rm -f full.log

if [ "$*" != "" ]; then
	langs="$@"
fi

function kompilethis {
	cd $top/$1
	if [ "cinf.k" -nt cinf-kompiled/main.maude ]; then
		echo -n "Kompiling in $1..."
		kompile cinf 2>&1 > kompile.run-all.log
		res=$?
		textres=`printresult $res`
		kompile_results="$kompile_results"$'\n'"$1: $textres"
		echo $textres
		cat kompile.run-all.log 2> /dev/null
	fi
}

function printresult {
	case $1 in
		0) echo "ok" ;;
		1) echo "FAIL" ;;
		*) echo "???" ;;
	esac
}

function docheck {
	check=${checks[$l]}
	case $check in
		skip)
			sed '/<k>/,/<\/k>/!d' $logname \
				| tr -d ' \n' \
				| grep -q "<k>skip</k>"
			textres=`printresult $?`
			skip_checks=$skip_checks$'\n'"$orig/$l/$p: $textres"
		;;
		thread) 
			sed '/<threads>/,/<\/threads>/!d' $logname \
				| tr -d ' \n' \
				| grep -q "<threads>.Bag</threads>"
			textres=`printresult $?`
			thread_checks=$thread_checks$'\n'$"$orig/$l/$p: $textres"
		;;
		*) 
	esac
}

for l in $langs; do
	echo -e "\n---------------- $l ----------------"

	kompilethis $orig/$l

	for p in `ls $tests 2> /dev/null`; do
		echo -e "\n*** test: $l/$p ***"
		cd $top/$orig/$l

		logname="${p}.krun.run-all.log"
		infile="$p.in"

		if [ -e "$infile" ]; then
			echo "(using input file $infile)"
			krun $p 2>&1 > $logname < $infile
		else
			krun $p 2>&1 > $logname
		fi

		krunres=$?
		textres="$orig/$l/$p: "`printresult $krunres`
		echo "krun of $textres"
		krun_results=$krun_results$'\n'$textres

		docheck

		cat $logname
		cd $top
	done
done


echo -e "\n============================================="
echo "Kompile results:"
echo "$kompile_results"

echo $'\n'
echo "Krun results:"
echo "$krun_results"

echo $'\n'
echo '"skip check" results:'
echo "$skip_checks"

echo $'\n'
echo '"thread check" results:'
echo "$thread_checks"

echo "============================================="

