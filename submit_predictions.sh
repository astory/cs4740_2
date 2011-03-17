#!/bin/bash
netid=$1
predictions_file=$2
curl -F predictions=@$predictions_file  --form-string "netid=$netid istest='1' isbaseline=1 degree=2 casefold=0 unkhandle=0 transsmoothing=None emitsmoothing=None" http://www.cs.cornell.edu/w8/~luwang/cs4740/getFile 
