#!/bin/bash
files=$2 #Input
csv=$1   #Output

echo trial,score >> $csv
sed -n -e 's/^score//p' -e s/^filename//p $files|tac|perl -pi -e 'tr/[\012\015]/e/d'|sed 's/%/\n/g'|sed -e s/^\ *//g|sed s/\ \ */,/ >> $csv
