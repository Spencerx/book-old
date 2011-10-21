Natural Language Processing for the Working Programmer

[Next](preface.xhtml)

* * * * *

### Daniël de Kok

`<me@danieldk.eu>`{.email}

### Harm Brouwer

`<me@hbrouwer.eu>`{.email}

Copyright © 2010, 2011 Daniël de Kok, Harm Brouwer

**License**

Some rights reserved. This book is made available under the Creative
Commons Attribution 3.0 License (CC-BY). This license is available from:
[http://creativecommons.org/licenses/by/3.0/](http://creativecommons.org/licenses/by/3.0/)

* * * * *

**Table of Contents**

[Preface](preface.xhtml)

[1. Acknowledgements](preface.xhtml#preface-ack)

[1. Introduction](chap-intro.xhtml)

[1.1. Welcome](chap-intro.xhtml#sec-intro-welcome)

[1.2. What is natural language
processing?](chap-intro.xhtml#sec-intro-whats-nlp)

[1.3. What is Haskell?](chap-intro.xhtml#sec-intro-whats-haskell)

[1.4. What you need](chap-intro.xhtml#sec-intro-needs)

[1.5. Ready, set, go!](chap-intro.xhtml#idp50224)

[2. Words](chap-words.xhtml)

[2.1. Introduction](chap-words.xhtml#sec-words-intro)

[2.2. Playing with words](chap-words.xhtml#sec-words-playwords)

[2.3. From words to sentences](chap-words.xhtml#sec-words-to-sentences)

[2.4. A note on tokenization](chap-words.xhtml#sec-words-tokenization)

[2.5. Word lists](chap-words.xhtml#sec-words-lists)

[2.6. Storing functions in a
file](chap-words.xhtml#sec-words-source-file)

[2.7. Word frequency lists](chap-words.xhtml#sec-words-freq-list)

[2.8. Monads](chap-words.xhtml#idp453264)

[2.9. Reading a text
corpus](chap-words.xhtml#sec-words-read-text-corpus)

[3. N-grams](chap-ngrams.xhtml)

[3.1. Introduction](chap-ngrams.xhtml#chap-ngrams-intro)

[3.2. Bigrams](chap-ngrams.xhtml#chap-ngrams-bigrams)

[3.3. A few words on Pattern
Matching](chap-ngrams.xhtml#chap-ngrams-pattern-matching)

[3.4. Collocations](chap-ngrams.xhtml#chap-ngrams-collocations)

[3.5. From bigrams to n-grams](chap-ngrams.xhtml#chap-ngrams-ngrams)

[3.6. Lazy and strict
evaluation](chap-ngrams.xhtml#chap-ngrams-lazy-strict)

[3.7. Suffix arrays](chap-ngrams.xhtml#chap-ngrams-suffixarrays)

[3.8. Markov models](chap-ngrams.xhtml#chap-ngrams-markov-models)

[4. Distance and similarity (proposed)](chap-similarity.xhtml)

[5. Classification](chap-classification.xhtml)

[5.1. Introduction](chap-classification.xhtml#chap-classification-intro)

[5.2. Naive Bayes classification](chap-classification.xhtml#idp1136832)

[5.3. Maximum entropy
classification](chap-classification.xhtml#idp1139760)

[6. Information retrieval (proposed)](chap-ir.xhtml)

[7. Part of speech tagging](chap-tagging.xhtml)

[7.1. Introduction](chap-tagging.xhtml#sec-tagging-intro)

[7.2. Frequency-based tagging](chap-tagging.xhtml#sec-tagging-frequency)

[7.3. Evaluation](chap-tagging.xhtml#sec-tagging-evaluation)

[7.4. Transformation-based tagging](chap-tagging.xhtml#sec-tagging-tbl)

[Bibliography](chap-tagging.xhtml#idp1482464)

[8. Regular languages (proposed)](chap-reglang.xhtml)

[9. Context-free grammars (Proposed)](chap-cfg.xhtml)

[10. Performance and efficiency (proposed)](chap-performance.xhtml)

**List of Figures**

3.1. [Constructing a suffix array](chap-ngrams.xhtml#fig-suffixarray)

3.2. [Linear search step](chap-ngrams.xhtml#fig-linear-search-step)

3.3. [Binary search step](chap-ngrams.xhtml#fig-binary-search)

5.1. [Linear and non-linear
classifiers](chap-classification.xhtml#fig-linear-nonlinear-classifier)

5.2. [Two competing
models](chap-classification.xhtml#fig-competing-classifiers)

**List of Tables**

7.1. [Performance of the frequency
tagger](chap-tagging.xhtml#tbl-tagging-freq-performance)

**List of Equations**

2.1. [Type-token ratio](chap-words.xhtml#idp365232)

3.1. [Difference between observed and expected
chance](chap-ngrams.xhtml#idp683216)

3.2. [Pointwise mutual information](chap-ngrams.xhtml#idp700880)

3.5. [Estimating the probability of a
sentence](chap-ngrams.xhtml#idp977904)

3.6. [The probability of a sentence as a Markov
chain](chap-ngrams.xhtml#idp992960)

3.8. [Approximation using the Markov
assumption](chap-ngrams.xhtml#idp1036688)

3.9. [The conditional probability of a word using the Markov
assumption](chap-ngrams.xhtml#idp1053248)

3.10. [The probability of a sentence using a bigram
model](chap-ngrams.xhtml#idp1072896)

5.1. [Calculating the empirical value of a
feature](chap-classification.xhtml#idp1148112)

5.2. [Calculating the expected value of a
feature](chap-classification.xhtml#idp1160960)

5.3. [Constraining the expected value to the empirical
value](chap-classification.xhtml#idp1177952)

7.1. [Transformation rule selection
criterion](chap-tagging.xhtml#idp1350384)

* * * * *

  -- -- -----------------------
        [Next](preface.xhtml)
        Preface
  -- -- -----------------------


