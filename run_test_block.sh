#!/bin/bash
startid=$1 #Integer
id=$startid
#for length in 1 2 3 4
for length in 100 1000 1000 996759 # wc -l pos_corpora/train.pos 
	do sh run_test.sh $length > $id.pos
	id=$(($id+1))
done
