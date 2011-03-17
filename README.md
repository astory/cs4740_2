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

A demo of the baseline system in a bash script.
It produces predictions formatted for submission
to the web form. Run the demo like this

`$ chmod +x run_tests.sh submit_predictions.sh`

`$ ./run_tests.sh > results/baseline.pos`

Also included is a compiled binary file for x86_64 in case you do not wish to
install Haskell.  The script runs this file by default; if want to compile it
yourself, simply run

`$ ghc --make Probabilities.hs`

and then run `run_tests.sh`.

Scoring
================

You can submit the resulting predictions like this

`$ ./submit_predictions.sh <netid> results/baseline.pos`

Save the emails with the submission results in plain text
as separate files (.eml) or as one file (.mbox), and run
the following command to put them in a convenient format

`$ ./tabulate_predictions.sh <emails> > results/results.csv`

The email file(s) may not contain spaces.

Data analysis
================

In order to ease the analysis, name the result .pos files
according to the identifiers in the experiment_design.R file.

Running plot_results.R will produce nice plots
