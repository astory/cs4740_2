#!/bin/bash

#Baseline
cat pos_corpora/train.pos |runhaskell Probabilities.hs| sed 's/("\([^"]*\)","\([^"]*\)")/\2 \1/'> results/baseline.pos
