#!/bin/bash

#Baseline
echo \ istest=1 isbaseline=1 degree=2 casefold=0 unkhandle=0 transsmoothing=None emitsmoothing=None
cat pos_corpora/train.pos |./Probabilities| sed -e 's/\\\\/\\/g' -e 's/("\([^"]*\)","\([^"]*\)")/\2 \1/'
