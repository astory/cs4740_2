#!/bin/bash
netid=$1
predictions_file=$2
params=`cat $predictions_file|sed -n '1 s/^/\ /p' | sed 's/\ / --form-string\ /g'`
sed -i 1d $predictions_file
curl -F predictions=@$predictions_file  --form-string netid=tkl22 $params  http://www.cs.cornell.edu/w8/~luwang/cs4740/getFile 

#istest=1 #0 or 1
#isbaseline=1 #0 or 1
#degree=2 #n-gram size: 2, 3 or 4 (4 or more)
#casefold=0 #0 or 1
#unkhandle=0 #0 or 1
#transsmoothing=None
#emitsmoothing=None
#'None' 'AddOne'  'GoodTuring' 'DI'  'Backoff' 'Other'


#curl -F predictions=@$predictions_file  --form-string netid=$netid --form-string istest=$istest --form-string isbaseline=$isbaseline --form-string degree=$degree --form-string casefold=$casefold --form-string unkhandle=$unkhandle --form-string transsmoothing=$transsmoothing --form-string emitsmoothing=$emitsmoothing http://www.cs.cornell.edu/w8/~luwang/cs4740/getFile

