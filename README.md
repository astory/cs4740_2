# Part-of-Speech Tagging in Haskell
## By Alec Story and Thomas Levine
Our system is written in Haskell. It depends on the following modules

*	Data.Char
*	Data.List
*	Data.List.Split
*	Data.Map
*	Data.Set
*	System.IO

Running the System
=================
The following sort of command will run the system

`run_tests.sh <training corpus size in words> > <output file>`.

For example

`run_tests.sh 100000 > foo.pos`.


Varying parameters
--------------------
Because we did not develop a command-line interface for passing
parameters, the file must be recompiled to test different
component arrangements

To compile the executable, run

`$ ghc --make Probabilities.hs`




Scoring
================

You can submit the resulting predictions like this

`$ ./submit_predictions.sh <netid> results/baseline.pos`

Save the emails with the submission results in plain text
as separate files (.eml) or as one file (.mbox), and run
the following command to put them in a convenient format

`$ ./tabulate_predictions.sh <emails> > results/results.csv`

The email file(s) may not contain spaces.

