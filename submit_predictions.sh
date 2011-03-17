#!/bin/bash
netid=$1
predictions_file=$2
curl -F predictions=@$predictions_file  --form-string netid=$netid \
--form-string istest=1 --form-string isbaseline=1 --form-string degree=2 \
--form-string casefold=0 --form-string unkhandle=0 \
--form-string transsmoothing=None --form-string emitsmoothing=None \
http://www.cs.cornell.edu/w8/~luwang/cs4740/getFile 
