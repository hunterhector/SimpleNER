10-710/11-763 HW1

This archive contains the following files:
  conlleval:                   evaluation script
  dev:                         reference tagger (4 columns)         - all features
  dev.simple_features:            "        "     "    "             - unigram words and bigram tags
  debug:                       debug output of the reference tagger - all features
  debug.simple_features:         "      "    "  "     "        "    - unigram words and bigram tags
  debug-local:                 local features of the reference tagger - all features
  debug-local.simple_features:   "      "      "  "     "        "    - unigram words and bigram tags
  test:                        test data
  train:                       CoNLL training data (in case you want to train your tagger)
  dev.human:                   human annotations for dev (in case you want them)
  sample.output:               sample output
  weights:                     weights file
  weights.simple_features:     weights trained with only unigrams words and bigram tags
  gazetteer.txt:               CoNLL 2003 gazetteer file
  README.txt:                  this file


To run the evaluation script do (on a Unix system):

    ./conlleval < sample.output

The eval script assumes that correct tags are in the fourth column, and the
output tags are in the fifth column.  So after you run your system on the dev
file, you should get 100% f-score (i.e. same as reference tagger).

The debug files are to help you debug your code.  For every sentence in dev,
'debug' contains the output your tagger should produce (5 columns), a newline,
and then a sorted list of the features of the output. The file 'debug-local' is
similar but prints the local features for each token.

'dev.simple_features', 'debug.simple_features', debug-local.simple_features' are
to help you debug your code without having to worry about implementing all the
features.  They contain the reference tagger run with only unigram and bigram
tag features turned-on, using the weights file 'weights.simple_features'.

If you find a bug in our tagger, let us know.

Good luck!

