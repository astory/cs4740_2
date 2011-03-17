#!/bin/bash
files="$1" #Input

echo file,score 
sed -n -e 's/^score//p' -e s/^filename//p $files|tac|perl -pi -e 'tr/[\012\015]/e/d'|sed 's/%/\n/g'|sed -e s/^\ *//g|sed s/\ \ */,/ 
