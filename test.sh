#!/bin/bash

name=plt-r5rs	# your name
command=plt-r5rs	# command line to run your interpreter
with_side_effect=y	# whether your interpreter supports syntaxes and procedures with side effect

## generate random test cases
if [[ ! -e gen ]]; then
	echo "Compiling gen"
	g++ -std=c++11 -o gen gen.cpp
fi
for depth in 2 4
do
	
	for (( i = 0; i < 10; ++i ))
	do
		if [[ ! -e random_test_${depth}_${i}.scm ]]; then
			./gen ${depth} ${depth} 10000 | cat > random_test_${depth}_${i}.scm
		fi
	done
done
rm -f ${name}.log

function _test()
{
	echo "Testing $*"

	if [[ ! -e $*.ans ]]; then 
		plt-r5rs $*.scm | cat > $*.ans
	fi
	${command} $*.scm | cat > tmp.out
	if [[ $? != 0 ]]; then
		echo "RE" | tee -a ${name}.log
	else
		diff $*.ans tmp.out &> /dev/null
		if [[ $? == 0 ]]; then
			echo "CORRECT" | tee -a ${name}.log
		else
			echo "WRONG" | tee -a ${name}.log
		fi
	fi
	rm -f tmp.out
}

_test no_side_effect
if [[ ${with_side_effect} == y ]]; then
	_test side_effect
fi


for depth in 2 4
do
	
	for (( i = 0; i < 10; ++i ))
	do
		_test random_test_${depth}_${i}
	done
done

_test deep_tree
