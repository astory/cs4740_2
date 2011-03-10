#!/bin/bash
#This returns the Haskell modules that are required
grep -h ^import *.hs|sed -e s/\ as.*// -e s/qualified// -e s/^import\ */\*\ / |sort -u

