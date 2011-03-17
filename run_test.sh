#!/bin/bash
head -n $1 pos_corpora/train.pos |runhaskell Probabilities.hs| sed -e 's/\\\\/\\/g' -e 's/("\([^"]*\)","\([^"]*\)")/\2 \1/'

