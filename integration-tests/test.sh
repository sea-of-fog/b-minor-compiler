declare -i NUM_OF_TESTS
NUM_OF_TESTS=3

# check if the compiler is built
if !(test -f ../bmc); then
	echo "Error: compiler not build"
	exit 1
fi

for i in $(seq 1 1 $NUM_OF_TESTS)
do

	# build the test program
	cd test$i
	../../bmc test.bm test.S
	gcc -no-pie test.S


	# check the answer
	./a.out > ans.out
	if cmp -s ans.out expected.ans; then
		echo "test $i correct"
	else 
		echo "test $i incorrect"
		exit 1
	fi

	# cleanup the test directory
	rm test.S ans.out a.out
	cd ..

done
