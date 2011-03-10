# Part-of-Speech Tagging in Haskell
## By Alec Story and Thomas Levine
Our system is written in Haskell. It depends on the following modules

*	Data.Char
*	Data.List
*	Data.List.Split
*	Data.Map
*	Data.Set
*	System.IO

A demo of the baseline system in a bash script.
It produces predictions formatted for submission
to the web form. Run the demo like this

`$ chmod +x run_tests.sh submit_predictions.sh`

`$ ./run_tests.sh > results/baseline.pos`

You can submit the resulting predictions like this

`$ ./submit_predictions.sh <netid> results/baseline.pos`

Also included is a compiled binary file for x86_64 in case you do not wish to
install Haskell.  The script runs this file by default; if want to compile it
yourself, simply run

`$ ghc --make Probabilities.hs`

and then run `run_tests.sh`.
