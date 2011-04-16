#!/bin/bash
head -n $1 pos_corpora/train.pos |`./Probabilities.b`| sed -e 's/\\\\/\\/g' -e 's/("\([^"]*\)","\([^"]*\)")/\2 \1/'

