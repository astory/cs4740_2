#!/bin/bash
netid=$1
predictions_file=$2
firstline=`sed -n '1 p' $predictions_file` 
params=`echo $firstline|sed 's/^/\ /' | sed 's/\ / --form-string\ /g'`
sed -i 1d $predictions_file
curl -F predictions=@$predictions_file  --form-string netid=$netid $params  http://www.cs.cornell.edu/w8/~luwang/cs4740/getFile 
sed -i "1i\
$firstline" $predictions_file

#istest=1 #0 or 1
#isbaseline=1 #0 or 1
#degree=2 #n-gram size: 2, 3 or 4 (4 or more)
#casefold=0 #0 or 1
#unkhandle=0 #0 or 1
#transsmoothing=None
#emitsmoothing=None
#'None' 'AddOne'  'GoodTuring' 'DI'  'Backoff' 'Other'


#curl -F predictions=@$predictions_file  --form-string netid=$netid --form-string istest=$istest --form-string isbaseline=$isbaseline --form-string degree=$degree --form-string casefold=$casefold --form-string unkhandle=$unkhandle --form-string transsmoothing=$transsmoothing --form-string emitsmoothing=$emitsmoothing http://www.cs.cornell.edu/w8/~luwang/cs4740/getFile

