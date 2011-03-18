# Part-of-Speech Tagging in Haskell
## By Alec Story and Thomas Levine
Our system is written in Haskell with some human interface
features in written in bash. The Haskell component depends
on the following modules.

*	Data.Char
*	Data.List
*	Data.List.Split
*	Data.Map
*	Data.Maybe
*	Data.Ratio
*	Data.Set
*	System.IO
*	System.IO.Unsafe
*	Data.MemoCombinators

Running the System
=================
The following sort of command will run the system

`sh run_test.sh <training corpus size in words> > <output file>`.

For example

`sh run_test.sh 100000 > foo.pos`.


Varying parameters
--------------------
Because we did not develop a command-line interface for passing
parameters, the Probabilities.hs file must be recompiled to test
different component arrangements. The relevant lines are below.

        let unk = True
            smooth = False -- add-one smoothing
            ...
            taggrams = safe_ngram_tally 2 tags

`unk` and `smooth` control unknown word handling and smoothing
respectively. Setting either to `True` will turn it on, and
setting it to `False` will turn it off.

The number in `safe_ngram_tally 2 tags` controls the n-gram length.
Set it to 2 to use bigrams, 3 to use trigrams, and so on.

After adjusting the parameters, compile the executable like so

`$ ghc --make Probabilities.hs`

and then run run_test.sh as explained before.


Scoring
================

You can submit the resulting predictions like this. (This
depends on curl.)

`$ sh submit_predictions.sh <netid> results/foo.pos`

Save the emails with the submission results in plain text
as separate files (.eml) or as one file (.mbox), and run
the following command to put them in a convenient format.

`$ sh tabulate_predictions.sh emails.mbox > results/results.csv`

The email filename(s) may not contain spaces.

