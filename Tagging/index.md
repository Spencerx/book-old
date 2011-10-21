Chapter 7. Part of speech tagging

[Prev](chap-ir.xhtml)

[Next](chap-reglang.xhtml)

* * * * *

## Chapter 7. Part of speech tagging

**Table of Contents**

[7.1. Introduction](chap-tagging.xhtml#sec-tagging-intro)

[7.2. Frequency-based tagging](chap-tagging.xhtml#sec-tagging-frequency)

[7.3. Evaluation](chap-tagging.xhtml#sec-tagging-evaluation)

[7.4. Transformation-based tagging](chap-tagging.xhtml#sec-tagging-tbl)

[Bibliography](chap-tagging.xhtml#idp1482464)

## 7.1. Introduction

In the last episode, you have seen how n-gram language models can be
used to model structure of language, purely based on words. In this
chapter, we will make a further abstraction and will try to find proper
part of speech tags (also named morphosyntactic tags) for words. Part of
speech tags give relevant information about the role of a word in its
narrow context. It may also provide information about the inflection of
a word. POS tags are a valuable part of a language processing pipeline,
they provide useful information to other components such as a parser or
a named-entity recognizer.

There is no such thing as a standard set of part of speech tags (let's
call them 'POS tags' from now on). Just like programming languages, text
editors, and operating systems, the tag set that people use depends on
the task at hand and taste. For our purposes, we will use the Brown tag
set^[[1](#ftn.idp1215152)]^.

This is a sentence from the Brown corpus that is annotated with tags:

A/AT similar/JJ resolution/NN passed/VBD in/IN the/AT Senate/NN by/IN
a/AT vote/NN of/IN 29-5/CD ./.

The notation here is very simple: as our previous fragments of the Brown
corpus the sentence is pre-tokenized. However, each word is amended by a
POS tag that indicate the role of the world. For instance, the word 'a'
is an article, 'similar' an adjective, and 'resolution' a singular
common noun.

Corpora, such as the Brown corpus only provide POS tags for a small
amount of sentences that occur in corpus. Being a working programmer,
you will deal with new data that does not occur in the Brown corpus.
Now, wouldn't it be nice to have a set of functions that could add POS
tags to untagged data? Software that performs this task is called a POS
tagger or morphosyntactic tagger, and this is exactly the thing we will
build in this chapter.

### 7.1.1. Exercises

-   In the data provided with this book, you will find the file
    `brown-pos-train.txt`{.filename}. Open this file with a text file
    viewer or text editor, and look at the five first sentences. Try to
    find out what the tags mean using the description of the Brown tag
    set.

## 7.2. Frequency-based tagging

In one of the simplest forms of tagging, we just assign the most
frequent POS tag for a token in the training data to a token in untagged
data. That's right, the most frequent tag, because a token can have more
than one tag. Consider the following two sentences:

-   I wouldn't **trust** him.

-   He put money in the family **trust**.

Both sentences contain the word 'trust'. However, 'trust' has different
roles in different roles in both sentences. In the first sentence
'trust' is a verb, in the second sentence it is a noun. So, for many
tokens we will have the choice of multiple tags. If we tag the token
with the most frequent tag, we will frequently tag tokens incorrectly,
but it is a first step.

To ease handling of tokens and tags, we will make type aliases for
tokens and tags and define a new datatype for training instances, aptly
named TrainingInstance:

~~~~ {.programlisting}
type Token = String
type Tag = String

data TrainingInstance = TrainingInstance Token Tag
                        deriving Show
~~~~

The Token and Tag aliases will allow us to write clean function
signatures. The TrainingInstance data type has only one constructor,
TrainingInstance. The data type derives from the Show typeclass, which
allows us to get a String representation of an
instance^[[2](#ftn.idp1235440)]^. We can use this constructor to create
training instances:

~~~~ {.haskell}
*Main> TrainingInstance "the" "AT"
TrainingInstance "the" "AT"
*Main> TrainingInstance "pony" "NN"
TrainingInstance "pony" "NN"
~~~~

Since our first POS tagger is trained purely on tokens and tags, and
requires no sentential information, the corpus will be represented as a
list of TrainingInstance. Since we can use the `words`{.function}
function to tokenize the corpus, the task at hand is to convert a list
of strings of the format "token/tag" to a list of TrainingInstance. This
is done by splitting the String on the forward slash character (/). We
can use the `break`{.function} function to break the string on the first
element for which the supplied function is true. For instance:

~~~~ {.haskell}
Prelude> break (== '/') "the/AT"
("the","/AT")
~~~~

This is a good start, we would only have to chop off the first character
of the second element in the tuple. However, there is another problem:
although a tag can never contain a slash, a token can. Consequently, we
should break the string on the last slash, rather than the first. A
cheap solution to this problem could be to reverse the string, applying
`break`{.function}, and then reversing the results again. We will take a
more sophisticated route, and write our own function:

~~~~ {.programlisting}
rsplit :: Eq a => a -> [a] -> ([a], [a])
rsplit sep l = let (ps, xs, _) = rsplit_ sep l in
               (ps, xs)

rsplit_ :: Eq a => a -> [a] -> ([a], [a], Bool)
rsplit_ sep = foldr (splitFun sep) ([], [], False)
    where splitFun sep e (px, xs, True) = (e:px, xs, True)
          splitFun sep e (px, xs, False)
                   | e == sep = (px, xs, True)
                   | otherwise = (px, e:xs, False)
~~~~

The core business happens in the `rsplit_`{.function} function, it
splits a list in the part before the last instance of `sep`{.varname}
(the prefix) and the part after (the suffix). It does this by folding
over the input list from right to left. The accumulator is a tuple that
holds the prefix list, the suffix list, and a Bool indicating whether
the separator was encountered. The function provided to the fold acts
upon this Bool:

-   If the Bool is True, the separator was seen, and the current element
    is added to the prefix list.

-   If the Bool is False, the separator was not seen yet. If the current
    element is equal to the separator, the Bool is changed to True to
    indicate that all remaining elements should be added to the prefix
    list. Otherwise, the element is added to the suffix list.

`rsplit`{.function} is just a tiny wrapper around `rsplit_`{.function}
that returns a binary tuple with just the prefix and suffix lists. The
`rsplit`{.function} function works as intended:

~~~~ {.haskell}
*Main> rsplit '/' "the/AT"
("the","AT")
*Main> rsplit '/' "a/b/TEST"
("a/b","TEST")
~~~~

We are now able to get the necessary data out of a String containing a
token and a tag. We can simply construct a training instance by
converting the tuple:

~~~~ {.programlisting}
toTrainingInstance :: String -> TrainingInstance
toTrainingInstance s = let (token, tag) = rsplit '/' s in
TrainingInstance token tag
~~~~

Why not see how we are doing, and get the ten first training instances
of the Brown corpus?

~~~~ {.haskell}
*Main> h <- IO.openFile "brown-pos-train.txt" IO.ReadMode
*Main> c <- IO.hGetContents h
*Main> take 10 $ map toTrainingInstance $ words c
[TrainingInstance "The" "AT",TrainingInstance "Fulton" "NP",
  TrainingInstance "County" "NN",TrainingInstance "Grand" "JJ",
  TrainingInstance "Jury" "NN",TrainingInstance "said" "VBD",
  TrainingInstance "Friday" "NR",TrainingInstance "an" "AT",
  TrainingInstance "investigation" "NN",TrainingInstance "of" "IN"]
~~~~

Alright! That's indeed our corpus in beautified format. The next step is
to traverse this corpus, registering for each word with which tag it
occurred (and how often). For this we write the
`tokenTagFreq`{.function} function:

~~~~ {.programlisting}
import qualified Data.List as L
import qualified Data.Map as M

tokenTagFreqs :: [TrainingInstance] -> M.Map Token (M.Map Tag Int)
tokenTagFreqs = L.foldl' countWord M.empty
    where
      countWord m (TrainingInstance token tag) = 
          M.insertWith (countTag tag) token (M.singleton tag 1) m
      countTag tag _ old = M.insertWith
          (\newFreq oldFreq -> oldFreq + newFreq) tag 1 old
~~~~

This function is very comparable to the `countElem`{.function} function
we saw earlier, the primary difference being that we have to handle two
levels of maps. Every Token in the first Map is associated with a value
that is itself a Map that maps Tags to frequencies (Int). If we have not
seen a particular Token yet, we will insert it to the map with the Token
as the key, the value is a map with just one key/value: the Tag
associated with the token and a frequency of one. If the Token was seen
before, we will update the frequency of the associated Tag, setting it
to one, if the Tag was never seen before with this token.

Let us test `tokenTagFreqs`{.function} on the first ten training
instances as well:

~~~~ {.haskell}
*Main> h <- IO.openFile "brown-pos-train.txt" IO.ReadMode
*Main> c <- IO.hGetContents h
*Main> tokenTagFreqs $ take 10 $ map toTrainingInstance $ words c
fromList [("County",fromList [("NN",1)]),("Friday",fromList [("NR",1)]),
  ("Fulton",fromList [("NP",1)]),("Grand",fromList [("JJ",1)]),
  ("Jury",fromList [("NN",1)]),("The",fromList [("AT",1)]),
  ("an",fromList [("AT",1)]),("investigation",fromList [("NN",1)]),
  ("of",fromList [("IN",1)]),("said",fromList [("VBD",1)])]
~~~~

It seems to work, but we cannot be sure until we have seen duplicates
and ambiguous tokens. You may want to play a little with larger corpus
samples or artificial training data to confirm that
`tokenTagFreqs`{.function} works as intended.

The next thing we need for our first part of speech tagger is use the
map defined by `tokenTagFreqs`{.function} to find the most frequent tag
for a word. This is a typical mapping situation: for each key/value pair
in the Map, we want to transform its value. The value was a Map, mapping
Tag to Int, and we want the value to be a Tag, namely the most frequent
Tag. There is also a `map`{.function} functions for Map:

~~~~ {.haskell}
*Main> :type Data.Map.map
Data.Map.map :: (a -> b) -> M.Map k a -> M.Map k b
~~~~

`Data.Map.map`{.function} accepts some function to map every value in a
Map to a new value. For getting the most frequent Tag, we have to fold
over the inner map, storing the most frequent tag and its frequency in
the accumulator. The Data.Map module provides the
`foldlWithKey`{.function} to fold over keys and values:

~~~~ {.haskell}
*Main> :type Data.Map.foldlWithKey
Data.Map.foldlWithKey :: (b -> k -> a -> b) -> b -> M.Map k a -> b
~~~~

This looks like the usual suspect, however, the folding function takes
an additional parameter. The folding function has the current
accumulator, the current key, and the associated value as its arguments.
Using these building blocks, we can construct the
`tokenMostFreqTag`{.function} function:

~~~~ {.programlisting}
tokenMostFreqTag :: M.Map Token (M.Map Tag Int) -> M.Map Token Tag
tokenMostFreqTag = M.map mostFreqTag
    where
      mostFreqTag = fst . M.foldlWithKey maxTag ("NIL", 0)
      maxTag acc@(maxTag, maxFreq) tag freq
                 | freq > maxFreq = (tag, freq)
                 | otherwise = acc
~~~~

The main function body uses `mostFreqTag`{.function} to get the most
frequent tag for each token. `mostFreqTag`{.function} folds over all
tokens and frequencies of a map associated with a token. The initial
value of the accumulator is the dummy tag 'NIL'. The `maxTag`{.function}
function that is used in the fold will replace the accumulator with the
current tag and its frequency if its frequency is higher than the
frequency of the tag in the accumulator. Otherwise, the tag in the
accumulator is more frequent, and the accumulator is retained. After
folding, we have the pair of the most frequent tag, and its frequency.
We use the `fst`{.function} function to get the first element of this
pair.

You can craft some examples to check whether
`tokenMostFreqTag`{.function} works as intended. For example:

~~~~ {.haskell}
*Main> tokenMostFreqTag $ tokenTagFreqs [TrainingInstance "a" "A",
  TrainingInstance "a" "B", TrainingInstance "a" "A"]
fromList [("a","A")]
~~~~

Combining `tokenTagFreqs`{.function} and `tokenMostFreqTag`{.function}
we can make a simple function to train our first tagging model from a
list of TrainingInstance:

~~~~ {.programlisting}
trainFreqTagger :: [TrainingInstance] -> M.Map Token Tag
trainFreqTagger = tokenMostFreqTag . tokenTagFreqs
~~~~

Next up is the actual tagger: it simply looks up a token, returning the
most frequent tag of a token. Since not all tags may be known, you may
want to decide how to handle unknown tags. For now, we will just return
the `Maybe                 Tag`{.function} type, allowing us to return
`Nothing`{.function} in the case we do not know how to tag a word. We
will define the function `freqTagWord`{.function} as a simple wrapper
around `Data.Map.lookup`{.function}:

~~~~ {.programlisting}
freqTagWord :: M.Map Token Tag -> Token -> Maybe Tag
freqTagWord m t = M.lookup w t
~~~~

We can now train our model from the Brown corpus, and tag some
sentences:

~~~~ {.haskell}
*Main> h <- IO.openFile "brown-pos-train.txt" IO.ReadMode
*Main> c <- IO.hGetContents h
*Main> let model = trainFreqTagger $ map toTrainingInstance $ words c
*Main> map (freqTagWord model) ["The","cat","is","on","the","mat","."]
[Just "AT",Just "NN",Just "BEZ",Just "IN",Just "AT",Just "NN",Just "."]
*Main> map (freqTagWord model) ["That's","right",",","the","mascara",
  "snake",".","Fast","and","bulbous",".","Also","a","tinned","teardrop","."]
[Just "DT+BEZ",Just "QL",Just ",",Just "AT",Just "NN",Just "NN",Just ".",
  Nothing,Just "CC",Nothing,Just ".",Just "RB",Just "AT",Nothing,
  Just "NN",Just "."]
~~~~

Isn't that NLP for the working programmer? Not only did you learn about
POS tagging, you built your own first POS tagger in just a few lines of
Haskell code. In the next section we will be a bit more scientific, and
focus on evaluation of taggers.

## 7.3. Evaluation

Now that you wrote your first tagger, the question is how well it work.
Not only to show it off to your colleagues (it will do relatively well),
but also to be able to see how future changes impact the performance of
the tagger. To check the performance of the tagger, we will use an
evaluation corpus. You should never evaluate a natural language
processing component on the training data, because it is easy to perform
well on seen data. Suppose that you wrote a tagger that just remembered
the training corpus exactly. This tagger would tag every word correctly,
but it will behave badly on unseen data.

For evaluating our taggers, we will use another set of sentences from
the Brown corpus. These annotated sentences are provided in
`brown-pos-test.txt`{.filename}. Since file has the same format as
`brown-pos-train.txt`{.filename}, it can also be read as a list of
TrainingInstance.

To evaluate a POS tagger, we will write a function that takes a tagging
function (Word -\> Maybe Tag) as its first argument and a training
corpus as its second argument. It should then return a tuple with the
total number of tokens in the corpus, the number of tokens that were
tagged correctly, and the number of tokens for which the tagger did not
provide an analysis (returned Nothing). This is the
`evalTagger`{.function} function:

~~~~ {.programlisting}
evalTagger tagFun = L.foldl' eval (0, 0, 0)
    where
      eval (n, c, u) (TrainingInstance token correctTag) =
          case tagFun token of
            Just tag -> if tag == correctTag then
                             (n+1, c+1, u)
                         else
                             (n+1, c, u)
            Nothing  -> (n+1, c, u+1)
~~~~

The function is pretty simple, it folds over all instances in the
evaluation data. The counts are incremented in the following manner:

-   If the tagger returned a tag for the current token, we have two
    options:

    -   The tagger picked the correct tag. We increment the number of
        tokens and the number of correct tags by one.

    -   The tagger picked an incorrect tag. We only increment the number
        of tokens by one.

-   The tagger returned no tag for the current token. We increment the
    number of tokens and the number of untagged tokens by one.

Time to evaluate your first tagger!

~~~~ {.haskell}
*Main> h <- IO.openFile "brown-pos-train.txt" IO.ReadMode
*Main> c <- IO.hGetContents h
*Main> let model = trainFreqTagger $ map toTrainingInstance $ words c
*Main> i <- IO.openFile "brown-pos-test.txt" IO.ReadMode
*Main> d <- IO.hGetContents i
*Main> evalTagger (freqTagWord model) $ map toTrainingInstance $ words d
(272901,239230,11536)
~~~~

Those are quite impressive numbers for a first try, 239230 / 272901 \*
100% = 87.66% of the tokens were tagged and tagged correctly. Of the
remaining 12.34% of the tokens, 11536 / 272901 \* 100% = 4.23% of the
words were not known. This means that we tagged 239230 / (272901 -
11536) \* 100% = 91.53% of the words known to our model correctly.

To get an impression what these numbers actually mean, we will create a
baseline. A baseline is a dumb model that indicates (more or less) the
range we are working in. Our baseline will simply pick the most frequent
tag for every token (as in, most frequent in the corpus, not for the
token). We will generalize the function a bit, allowing us to specify
the tag to be used:

~~~~ {.programlisting}
baselineTagger :: Tag -> Token -> Maybe Tag
baselineTagger tag _ = Just tag
~~~~

The most frequent tag in the Brown corpus is NN, the singular common
noun. Let's evaluate the baseline tagger:

~~~~ {.haskell}
*Main> h <- IO.openFile "brown-pos-test.txt" IO.ReadMode
*Main> c <- IO.hGetContents h
*Main> evalTagger (baselineTagger "NN") $ map toTrainingInstance $ words c
(272901,31815,0)
~~~~

We sure do a lot better than this baseline at 31815 / 272901 \* 100% =
11.66%! What if we implement the same heuristic for unknown words in our
frequency tagger? You may expect it to only correct a small proportion
of unknown words, but trying never hurts. We add a function named
`backOffTagger`{.function} that wraps a tagger, returning some default
tag if the tagger failed to find a tag for a token:

~~~~ {.programlisting}
backoffTagger :: (Token -> Maybe Tag) -> Tag -> Token -> Maybe Tag
backoffTagger f bt t = let pick = f t in
                       case pick of
                         Just tag -> Just tag
                         Nothing  -> Just bt
~~~~

See how we can nicely cascade taggers by writing higher-order functions?
We proceed to evaluate this tagger:

~~~~ {.haskell}
*Main> h <- IO.openFile "brown-pos-train.txt" IO.ReadMode
*Main> c <- IO.hGetContents h
*Main> let model = trainFreqTagger $ map toTrainingInstance $ words c
*Main> i <- IO.openFile "brown-pos-test.txt" IO.ReadMode
*Main> d <- IO.hGetContents i
*Main> evalTagger (backoffTagger (freqTagWord model) "NN") $
  map toTrainingInstance $ words d
(272901,241590,0)
~~~~

That did improve performance some. Of the 11536 tokens that we did not
tag in the frequency-based tagger, we now tagged 2360 tokens correctly.
This means that we tagged 20.46% of the unknown words correctly. This is
almost double of the baseline, how is that possible? It turns out that
of some classes of tokens, such as articles, prepositions, and tokens,
you will never encounter new ones in unseen data. Unknown words are
often nouns, verbs, and adjectives. Since nouns form a larger
proposition of unknown words than all words, you will also get a better
performance when guessing that a word is a singular common noun in
unseen data.

[Table 7.1, “Performance of the frequency
tagger”](chap-tagging.xhtml#tbl-tagging-freq-performance) summarizes the
result so far. We have also added the score of the oracle this is the
performance that you would attain if the tagger was omniscient. In this
task, the oracle performs the task perfectly, but this is not true for
every task.

**Table 7.1. Performance of the frequency tagger**

Tagger

Accuracy (%)

Baseline

11.66

Frequency-based

87.66

Frequency-based + backoff

88.53

Oracle

100.00

\

## 7.4. Transformation-based tagging

While the frequency-tagger that you developed over the last two sections
was a good first attempt at POS tagging, the performance of taggers can
be improved by taking context into account. To give an example, the
token 'saving' is used as a verb most frequently. However, when the word
is used as a noun, this can often be derived from the context, as in the
following two sentences:

1.  The/AT weight/NN advantage/NN ,/, plus/CC greater/JJR durability/NN
    of/IN the/AT plastic/NN unit/NN ,/, yields/VBZ a/AT **saving/NN**
    of/IN about/RB one-fifth/NN in/IN shipping/VBG ./.

2.  Its/PP$ elimination/NN would/MD result/VB in/IN the/AT **saving/NN**
    of/IN interest/ NN costs/NNS ,/, heavy/JJ when/WRB short-term/NN
    money/NN rates/NNS are/BER high /JJ ,/, and/CC in/IN freedom/NN
    from/IN dependence/NN on/IN credit/NN which/WDT is/BEZ not/\*
    always/RB available/JJ when/WRB needed/VBN most/RBT ./.

In both cases, saving is preceded by an article and succeeded by a
preposition. The context disambiguates what specific reading of the
token 'saving' should be used.

We could manually inspect all errors in the training corpus after
tagging it with the frequency-based tagger, and write rules that correct
mistaggings. This has been done in the past, and can give a tremendous
boost in performance. Unfortunately, finding such rules is very tedious
work, and specific to one language and tag set. Fortunately,
[[bib-brill1992]](chap-tagging.xhtml#bib-brill1992) has shown that such
rules can be learnt automatically using so-called transformation-based
learning. The learning procedure is straightforward:

1.  Tag every token in the training corpus using the most frequent tag
    for a word.

2.  Create rules from rule templates that correct incorrectly tagged
    words.

3.  Count how many corrections were made and errors were introduced when
    each rule is applied to the corpus.

4.  Select the best rule according to the following equation:

    **Equation 7.1. Transformation rule selection criterion**

    argmax r ∈ R score(r) = | correctr | - | errorr |

    \

5.  Go to step 2, unless a threshold has been reached (e.g. rules do not
    give a net improvement).

The rule templates follow a very simple format. These are two examples
from Brill's paper:

1.  old\_tag new\_tag NEXT-TAG tag

2.  old\_tag new\_tag PREV-TAG to

Two possible rules derived from these rule templates are:

1.  TO IN NEXT-TAG AT

2.  NN VB PREV-TAG TO

The first rule replaces the tag 'TO' (infinitival 'to') by 'IN'
(preposition) if the next tag is 'AT' (article). The second rule,
replaces the tag 'NN' (singular common noun) to 'VB' (verb, base) if the
previous tag was 'TO' (infinitival 'to'). As you can immediately see,
these are two very effective rules.

Since you already have a frequency-based tagger, you can already perform
the first step of the learning procedure for transformation-based
tagging. What we still need are rule templates, rule extractors, and a
scoring function. For brevity, we will only focus on three tag-based
templates, but after implementing the learning procedure, it should be
fairly obvious how to had other contexts and integrating words in
templates. The templates that we will create, will take be all
variations on directly surrounding tags (previous tag, next tag, both
surrounding tags). Thanks to Haskell's algebraic data types, we can
easily model these templates:

~~~~ {.programlisting}
data Replacement = Replacement Tag Tag
                   deriving (Eq, Ord, Show)

data TransformationRule = 
      NextTagRule     Replacement Tag
    | PrevTagRule     Replacement Tag
    | SurroundTagRule Replacement Tag Tag
      deriving (Eq, Ord, Show)
~~~~

To confirm that these templates are indeed working as expected, we can
recreate the rules that were mentioned earlier:

~~~~ {.haskell}
*Main> NextTagRule (Replacement "TO" "IN") "AT"
NextTagRule (Replacement "TO" "IN") "AT"
*Main> PrevTagRule (Replacement "NN" "VB") "TO"
PrevTagRule (Replacement "NN" "VB") "TO"
~~~~

Awesome! Now on to rule instantiation. We need to instantiate rules for
tags that are incorrect, so ideally we have the corpus represented as a
list of binary tuples, where the first element is the correct tag, and
the second element the tag that is currently assigned by the tagger. For
instance:

~~~~ {.programlisting}
[("AT","AT"),("NN","VB"),("TO", "TO")]
~~~~

This can simply be done by using Haskell's `zip`{.function} function,
that 'zips' together two lists into one list of binary tuples:

~~~~ {.haskell}
*Main> :type zip
zip :: [a] -> [b] -> [(a, b)]
*Main> let correct = ["AT","NN","TO"]
*Main> let tagged = ["AT","VB","TO"]
*Main> zip correct tagged
[("AT","AT"),("NN","VB"),("TO","TO")]
~~~~

However, using lists is not really practical in this case. By the way
they are normally traversed, the current element is always the head,
meaning that we do not readily have access to previous elements. But we
no need to access previous elements for the PrevTagRule and
SurroundTagRule templates. We can write our own function that keeps
track of previous elements, but a package with such functionality,
called ListZipper, is already available. After using **cabal** to
install the ListZipper package, you will have access to the
Data.List.Zipper module. A Zipper can be seen as a list that can be
traversed in two directions. We can construct a Zipper from a list:

~~~~ {.haskell}
*Main> let taggingState = Data.List.Zipper.fromList $
  zip ["AT","NN","TO"]  ["AT","VB","TO"]
*Main> taggingState
Zip [] [("AT","AT"),("NN","VB"),("TO","TO")]
~~~~

We can get the current element (the element the so-called cursor is
pointing at) in the zipper using `Data.List.Zipper.cursor`{.function}:

~~~~ {.haskell}
*Main> Data.List.Zipper.cursor taggingState
("AT","AT")
~~~~

We can move the cursor to the left (point to the previous element) with
`Data.List.Zipper.left`{.function}, and to the right (point to the next
element) with `Data.List.Zipper.right`{.function}:

~~~~ {.haskell}
*Main> Data.List.Zipper.right taggingState
Zip [("AT","AT")] [("NN","VB"),("TO","TO")]
*Main> Data.List.Zipper.cursor $ Data.List.Zipper.right taggingState
("NN","VB")
*Main> Data.List.Zipper.left $ Data.List.Zipper.right $ taggingState
Zip [] [("AT","AT"),("NN","VB"),("TO","TO")]
*Main> Data.List.Zipper.cursor $ Data.List.Zipper.left $
  Data.List.Zipper.right $ taggingState
("AT","AT")
~~~~

This allows us to do the kind of maneuvering necessary to extract rules.
The rule instantiations are modelled as functions, and are pretty
simple: they just pick the information that is necessary out of their
environment. We have to bit careful at the boundaries of the Zipper
though: at the beginning of the Zipper only NextTagRule can extract the
necessary information, and at the end of the Zipper this applies to
PrevTagRule. To be able to handle such situations, we make the return
type of the instantiation functions Maybe TransformationRule. Let's go
through the instantiation functions one by one, starting with
`instNextTagRule0`{.function} (we add the '0' suffix, since we will
prettify these functions later):

~~~~ {.programlisting}
import qualified Data.List.Zipper as Z

instNextTagRule0 :: Z.Zipper (Tag, Tag) -> Maybe TransformationRule
instNextTagRule0 z
    | Z.endp z = Nothing
    | Z.endp $ Z.right z = Nothing
    | otherwise = Just $ NextTagRule (Replacement incorrectTag correctTag) nextTag
    where (correctTag, incorrectTag) = Z.cursor z
          nextTag = snd $ Z.cursor $ Z.right z
~~~~

When instantiating a rule from the current element in the Zipper, we
have two problematic conditions to check for. The first is that the
Zipper does not point to an element. This happens when we would traverse
to the right when are already at the last element of the Zipper. In the
second condition, we are actually at the last element of the Zipper. In
this situation, we cannot extract the next Tag. For both conditions, we
return Nothing. When these conditions do not hold, we can extract a
NextTagRule. We do this by defining the replacement, replacing the
incorrect tag by the correct one, and extracting the next tag. We can
test this instantiation function, assuming that `taggingState`{.varname}
is defined as above:

~~~~ {.haskell}
*Main> instNextTagRule0 $ Data.List.Zipper.right taggingState
Just (NextTagRule (Replacement "VB" "NN") "TO")
~~~~

The `instPrevTag0`{.function} function is almost similar, except that in
the second condition returns Nothing if the current element is the first
element of the Zipper. And, of course, we extract the previous tag
rather than the next tag:

~~~~ {.programlisting}
instPrevTagRule0 :: Z.Zipper (Tag, Tag) -> Maybe TransformationRule
instPrevTagRule0 z
    | Z.endp z = Nothing
    | Z.beginp z = Nothing
    | otherwise = Just $ PrevTagRule (Replacement incorrectTag correctTag) prevTag
    where (correctTag, incorrectTag) = Z.cursor z
          prevTag = snd $ Z.cursor $ Z.left z
~~~~

Let's do a sanity check to be safe:

~~~~ {.haskell}
*Main> instPrevTagRule0 $ Data.List.Zipper.right taggingState
Just (PrevTagRule (Replacement "VB" "NN") "AT")
~~~~

Finally, we write the `instSurroundTag0`{.function} function, which
combines the functionality of `instNextTag0`{.function} and
`instPrevTag0`{.function}:

~~~~ {.programlisting}
instSurroundTagRule0 :: Z.Zipper (Tag, Tag) -> Maybe TransformationRule
instSurroundTagRule0 z
    | Z.endp z = Nothing
    | Z.beginp z = Nothing
    | Z.endp $ Z.right z = Nothing
    | otherwise = Just $ SurroundTagRule (Replacement incorrectTag correctTag)
                  prevTag nextTag
    where (correctTag, incorrectTag) = Z.cursor z
          prevTag = snd $ Z.cursor $ Z.left z
          nextTag = snd $ Z.cursor $ Z.right z
~~~~

And this also works as intended:

~~~~ {.haskell}
*Main> instSurroundTagRule0 $ Data.List.Zipper.right taggingState
Just (SurroundTagRule (Replacement "VB" "NN") "AT" "TO")
~~~~

We will make these functions simpler by making use of the Maybe monad.
First, we define two functions to get the previous and the next element
of the zipper, wrapped in Maybe. To accomplish this, we use the
`safeCursor`{.function} function, which returns the element the cursor
points at using Maybe. It will return value Nothing if the cursor points
beyond the last element of the zipper.

~~~~ {.programlisting}
rightCursor :: Z.Zipper a -> Maybe a
rightCursor = Z.safeCursor . Z.right

leftCursor :: Z.Zipper a -> Maybe a
leftCursor z = if Z.beginp z then
                   Nothing
               else
                   Z.safeCursor $ Z.left z
~~~~

The `rightCursor`{.function} function is trivial. The
`leftCursor`{.function} is a bit more complicated, since calling
`left`{.function} on a Zipper with a cursor pointing at the first
element, will return an equivalent Zipper. So, we return Nothing when we
are pointing at the first element (and cannot move left).

In our previous implementations of the instantiation functions, we
checked all failure conditions using guards. However, once we work with
expressions evaluating to Maybe, we can use the Maybe monad instead. The
Maybe monad represents computations that could fail (return Nothing),
and a failure will be propagated (the monad will end in Nothing). The
`return`{.function} function is used to pack the value of the final
expression in a Maybe.

Using the Maybe monad, we can simplify the instantiation functions:

~~~~ {.programlisting}
instNextTagRule :: Z.Zipper (Tag, Tag) -> Maybe TransformationRule
instNextTagRule z = do
    (_, next) <- rightCursor z
    (correct, incorrect) <- Z.safeCursor z
    return $ NextTagRule (Replacement incorrect correct) next

instPrevTagRule :: Z.Zipper (Tag, Tag) -> Maybe TransformationRule
instPrevTagRule z = do
    (_, prev)            <- leftCursor z
    (correct, incorrect) <- Z.safeCursor z
    return $ PrevTagRule (Replacement incorrect correct) prev


instSurroundTagRule :: Z.Zipper (Tag, Tag) -> Maybe TransformationRule
instSurroundTagRule z = do
    (_, next)            <- rightCursor z
    (_, prev)            <- leftCursor z
    (correct, incorrect) <- Z.safeCursor z
    return $ SurroundTagRule (Replacement incorrect correct) prev next
~~~~

With the instantiation functions set in place, we can fold over the
Zipper using `Data.List.Zipper.foldlz'`{.function}. This is a left fold
with a strict accumulator. The folding function gets the accumulator as
its first argument and the current Zipper (state) as its second:

~~~~ {.haskell}
*Main> :type Data.List.Zipper.foldlz'
Data.List.Zipper.foldlz'
  :: (b -> Z.Zipper a -> b) -> b -> Z.Zipper a -> b
~~~~

Using this function, we write the `instRules0`{.function} function:

~~~~ {.programlisting}
instRules0 :: [(Z.Zipper (Tag, Tag) -> Maybe TransformationRule)] ->
             Z.Zipper (Tag, Tag) -> S.Set TransformationRule
instRules0 funs = Z.foldlz' applyFuns S.empty
    where applyFuns s z
              | correct == proposed = s
              | otherwise = foldl (applyFun z) s funs
              where (correct, proposed) = Z.cursor z
                    applyFun z s f = case f z of
                                     Nothing -> s
                                     Just r -> S.insert r s
~~~~

`instRules0`{.function} accepts a list of instantiation functions
(`funs`{.varname}) and a zipper, and returns a Set of instantiated
rules. It folds over the zipper, applying all functions
(`applyFuns`{.function}) to the current element. If the tag that is
currently proposed is already correct, the Set is unchanged, because
there is no transformation to be learnt. If the proposed tag differs
from the correct tag, rules are instantiated by folding over the
instantiation functions. Applying this to our little test data, shows
that the function is operating correctly:

~~~~ {.haskell}
*Main> instRules0 [instNextTagRule, instPrevTagRule, instSurroundTagRule]
  taggingState
fromList [NextTagRule (Replacement "VB" "NN") "TO",
  PrevTagRule (Replacement "VB" "NN") "AT",
  SurroundTagRule (Replacement "VB" "NN") "AT" "TO"]
~~~~

Now we have to massage the corpus and the proposed corpus to the correct
format. The `initialLearningState`{.function} function extracts the list
of correct tags from the corpus, and uses the word frequency tagger with
'NN' as the back-off for unknown words to get a list of proposed tags.
Both lists are then zipped and the zipped list is converted to a Zipper:

~~~~ {.programlisting}
initialLearningState :: [TrainingInstance] -> Z.Zipper (Tag, Tag)
initialLearningState train = Z.fromList $ zip (correct train) (proposed train)
    where proposed    = map tagger . trainTokens
          correct     = map (\(TrainingInstance _ tag) -> tag)
          tagger      = DM.fromJust . backoffTagger (freqTagWord model) "NN"
          trainTokens = map (\(TrainingInstance token _) -> token)
          model       = trainFreqTagger train
~~~~

Now we can use this function to create the initial state for the
transformation-based learner, and extract all possible transformation
rules:

~~~~ {.haskell}
*Main> h <- IO.openFile "brown-pos-train.txt" IO.ReadMode
*Main> c <- IO.hGetContents h
*Main> let proposedRules = instRules0 [instNextTagRule, instPrevTagRule, instSurroundTagRule] $
  initialLearningState $ map toTrainingInstance $ words c
*Main> Data.Set.size proposedRules
18992
~~~~

Good, this allows us to find all possible correction rules. We could now
calculate the scores for all rules. But the rule selection can be made
somewhat more efficient. Each rule instantiation was actually an
instance of a correct rule application. If we register the correct
counts, we can start with scoring the most promising rules first. Once
the score of a rule is higher than the correct count of the next rule,
we have found the most effective rule. We can modify
`instRules0`{.function} to do this:

~~~~ {.programlisting}
instRules :: [(Z.Zipper (Tag, Tag) -> Maybe TransformationRule)] ->
             Z.Zipper (Tag, Tag) -> M.Map TransformationRule Int
instRules funs = Z.foldlz' applyFuns M.empty
    where applyFuns m z
              | correct == proposed = m
              | otherwise = foldl (applyFun z) m funs
              where (correct, proposed) = Z.cursor z
                    applyFun z m f = case f z of
                                       Nothing -> m
                                       Just r -> M.insertWith' (+) r 1 m
~~~~

We then use this frequency map to create a list of rules sorted by
frequency:

~~~~ {.programlisting}
sortRules :: M.Map TransformationRule Int -> [(TransformationRule, Int)]
sortRules = L.sortBy (\(_,a) (_,b) -> compare b a) . M.toList
~~~~

`Data.List.sortBy`{.function} sorts a list according to some comparison
function. We use the stock `compare`{.function} function:

~~~~ {.haskell}
Prelude> :type compare
compare :: (Ord a) => a -> a -> Ordering
Prelude> compare 1 2
LT
Prelude> compare 2 2
EQ
Prelude> compare 2 1
GT
~~~~

The compare function returns a value of the Ordering type, returning LT,
EQ, GT, depending on whether the first argument is smaller than, equal
to, or larger than the second argument. In `sortRules`{.function}, we
use a lambda to get the second tuple element (representing a frequency).
We also swap the arguments to `compare`{.function} function to get a
reverse ordering, making larger elements come first.

Let's get some immediate gratification by extracting the ten rules with
the most corrections:

~~~~ {.haskell}
*Main> h <- IO.openFile "brown-pos-train.txt" IO.ReadMode
*Main> c <- IO.hGetContents h
*Main> let proposedRules = instRules [instNextTagRule, instPrevTagRule, instSurroundTagRule] $
  initialLearningState $ map toTrainingInstance $ words c
*Main> take 10 $ sortRules proposedRules
[(NextTagRule (Replacement "TO" "IN") "AT",3471),
  (PrevTagRule (Replacement "TO" "IN") "NN",2459),
  (PrevTagRule (Replacement "NN" "VB") "TO",1690),
  (NextTagRule (Replacement "VBN" "VBD") "AT",1154),
  (PrevTagRule (Replacement "TO" "IN") "VBN",1088),
  (SurroundTagRule (Replacement "TO" "IN") "NN" "AT",1034),
  (NextTagRule (Replacement "TO" "IN") "NN",994),
  (NextTagRule (Replacement "NN" "VB") "AT",846),
  (PrevTagRule (Replacement "TO" "IN") "JJ",813),
  (NextTagRule (Replacement "VBD" "VBN") "IN",761)]
~~~~

The next thing we need to be able to do is to evaluate a rule. However,
we currently have no way to see whether a rule applies. To this end, we
write the `ruleApplication`{.function} function, this function returns
the replacement tag wrapped in Maybe's Just constructor. If the rule
could not be applied to the current corpus element, Nothing is returned:

~~~~ {.programlisting}
ruleApplication :: TransformationRule -> Z.Zipper (Tag, Tag) -> Maybe Tag
ruleApplication (NextTagRule (Replacement old new) next) z = do
  (_, proposed)     <- Z.safeCursor z
  (_, nextProposed) <- rightCursor z
  if proposed == old && nextProposed == next then Just new else Nothing
ruleApplication (PrevTagRule (Replacement old new) prev) z = do
  (_, proposed)     <- Z.safeCursor z
  (_, prevProposed) <- leftCursor z
  if proposed == old && prevProposed == prev then Just new else Nothing
ruleApplication (SurroundTagRule (Replacement old new) prev next) z = do
  (_, proposed)     <- Z.safeCursor z
  (_, nextProposed) <- rightCursor z
  (_, prevProposed) <- leftCursor z
  if proposed == old && prevProposed == prev &&
      nextProposed == next then Just new else Nothing
~~~~

This function closely matches the instantiation functions, except that
we check whether the context corresponds to the context specified by the
rule. We can then apply a rule to every element in the Zipper, checking
whether the change was correct when a Just value is returned:

~~~~ {.programlisting}
scoreRule :: TransformationRule -> Z.Zipper (Tag, Tag) -> Int
scoreRule r z = nCorrect - nIncorrect
    where (nCorrect, nIncorrect) = scoreRule_ r z

scoreRule_ :: TransformationRule -> Z.Zipper (Tag, Tag) -> (Int, Int)
scoreRule_ r = Z.foldlz' (scoreElem r) (0, 0)
    where scoreElem r s@(nCorrect, nIncorrect) z =
              case ruleApplication r z of
                Just tag -> if tag == correct then
                                (nCorrect + 1, nIncorrect)
                            else
                                (nCorrect, nIncorrect + 1)
                Nothing  -> s
              where (correct, _) = Z.cursor z
~~~~

The main action happens in the `scoreRule_`{.function} function. It
traverses the zipper, applying the rule to each element. If the rule
applies to an element, we check whether the application corrected the
tag, and update the counts (`nCorrect`{.varname} and
`nIncorrect`{.varname}) accordingly. If the rule does not apply, we keep
the counts as they are. `scoreRule`{.function} is just a simple wrapper
around `scoreRule_`{.function}, and subtracts the number of errors
introduced from the number of corrections. You can try to apply this
function to some rules, for instance the best rule of the initial
ranking:

~~~~ {.haskell}
*Main> h <- IO.openFile "brown-pos-train.txt" IO.ReadMode
*Main> c <- IO.hGetContents h
*Main> let learningState = initialLearningState $ map toTrainingInstance $ words c
*Main> let proposedRules = instRules
  [instNextTagRule, instPrevTagRule, instSurroundTagRule] learningState
*Main> head $ sortRules proposedRules
(NextTagRule (Replacement "TO" "IN") "AT",3471)
*Main> let (firstRule, _) = head $ sortRules proposedRules
*Main> scoreRule firstRule learningState
3470
~~~~

So, given a set of rules, we have to select the best rule. We know that
we have found the best rule when its score is higher than the number of
corrections of the next rule in the sorted rule list. The
`selectRule_`{.function} function does exactly this:

~~~~ {.programlisting}
selectRule :: [(TransformationRule, Int)] -> Z.Zipper (Tag, Tag) ->
              (TransformationRule, Int)
selectRule ((rule, _):xs) z = selectRule_ xs z (rule, (scoreRule rule z))

selectRule_ :: [(TransformationRule, Int)] -> Z.Zipper (Tag, Tag) ->
              (TransformationRule, Int) -> (TransformationRule, Int)
selectRule_ [] _ best = best
selectRule_ ((rule, correct):xs) z best@(bestRule, bestScore) =
    if bestScore >= correct then
        best
    else
        if bestScore >= score then
            selectRule_ xs z best
        else
            selectRule_ xs z (rule, score)
    where score = scoreRule rule z
~~~~

First we check whether the stopping condition is reached
(`bestScore >                 correct`{.code}). If this is not the case,
we have to decide whether the currently best rule is better than the
current rule (`bestScore >= score)`{.code}. If this is is not the case,
the current rule becomes the best rule. Let us use this function to
select the best rule:

~~~~ {.haskell}
*Main> h <- IO.openFile "brown-pos-train.txt" IO.ReadMode
*Main> c <- IO.hGetContents h
*Main> let learningState = initialLearningState $ map toTrainingInstance $ words c
*Main> let proposedRules = instRules
  [instNextTagRule, instPrevTagRule, instSurroundTagRule] learningState
*Main> selectRule (sortRules proposedRules) learningState
(NextTagRule (Replacement "TO" "IN") "AT",3470)
~~~~

Excellent! Our learner is now almost done! Once we have selected a rule,
we need to update the training state, and then we can rinse and repeat
until we are happy with the list of rules. First, we will make the
`updateState`{.function} function to update the learning state:

~~~~ {.programlisting}
updateState :: TransformationRule -> Z.Zipper (Tag, Tag) ->
               Z.Zipper (Tag, Tag)
updateState r = Z.fromList . reverse . Z.foldlz' (update r) []
    where update r xs z =
              case ruleApplication r z of
                Just tag -> (correct, tag):xs
                Nothing  -> e:xs
              where e@(correct, _) =  Z.cursor z
~~~~

The updated state is created by copying the old state using a fold,
replacing the proposed tag if a rule is applicable (returns Just tag).
We used a strict left-fold for efficiency. Since we are building a list,
the consequency is that we construct the list in reverse order. We then
reverse the list, and construct a Zipper from this list. The next
function, `transformationRules`{.function}, constructs the list of
transformations:

~~~~ {.programlisting}
transformationRules :: [(Z.Zipper (Tag, Tag) -> Maybe TransformationRule)] ->
                       Z.Zipper (Tag, Tag) -> [TransformationRule]
transformationRules funs state = bestRule:(transformationRules funs nextState)
    where (bestRule, _) = selectRule (sortRules $ instRules funs state) state
          nextState     = updateState bestRule state
~~~~

This function is fairly simple: during each recursion we find the next
best rule, and update the state accordingly. The rule becomes the head
of the list that we are returning, and we call
`transformationRules`{.function} recursively to construct the tail of
the list. We have now completed our transformation-based learner! Time
to extract some rules:

~~~~ {.haskell}
*Main> h <- IO.openFile "brown-pos-train.txt" IO.ReadMode
*Main> c <- IO.hGetContents h
*Main> let learningState = initialLearningState $ map toTrainingInstance $ words c
*Main> take 10 $ transformationRules [instNextTagRule, instPrevTagRule, instSurroundTagRule]
  learningState
[NextTagRule (Replacement "TO" "IN") "AT",
  PrevTagRule (Replacement "NN" "VB") "TO",
  NextTagRule (Replacement "TO" "IN") "NP",
  PrevTagRule (Replacement "VBN" "VBD") "PPS",
  PrevTagRule (Replacement "NN" "VB") "MD",
  NextTagRule (Replacement "TO" "IN") "PP$",
  PrevTagRule (Replacement "VBN" "VBD") "NP",
  PrevTagRule (Replacement "PPS" "PPO") "VB",
  NextTagRule (Replacement "TO" "IN") "JJ",
  NextTagRule (Replacement "TO" "IN") "NNS"]
~~~~

Since we haven't optimized our implementation for instructional
purposes, extracting the ten most effective rules can take a while. It
is best to store the resulting the source file if you do not want to
repeat this step:

~~~~ {.programlisting}
tenBestRules :: [TransformationRule]
tenBestRules = [NextTagRule (Replacement "TO" "IN") "AT",
                PrevTagRule (Replacement "NN" "VB") "TO",
                NextTagRule (Replacement "TO" "IN") "NP",
                PrevTagRule (Replacement "VBN" "VBD") "PPS",
                PrevTagRule (Replacement "NN" "VB") "MD",
                NextTagRule (Replacement "TO" "IN") "PP$",
                PrevTagRule (Replacement "VBN" "VBD") "NP",
                PrevTagRule (Replacement "PPS" "PPO") "VB",
                NextTagRule (Replacement "TO" "IN") "JJ",
                NextTagRule (Replacement "TO" "IN") "NNS"]
~~~~

The tagger itself recursively applies every rule to the proposed tags.

### 7.4.1. Exercises

1.  Add two additional types of rules:

    -   PrevOneOrTwoTagRule: this rule is triggered when one of the last
        or second to last tags corresponds to the specified tag.

    -   PrevOneOrTwoTagRule: this rule is triggered when one of the two
        next tags corresponds to the specified tag.

    Extract the ten best rules, adding these rule types.

2.  Modify the examples so that rules can condition on words as well.
    Implement three rule types, CurWordRule, PrevWordRule, NextWordRule,
    that condition respectively on the current, previous and next word.

    Extract the ten best rules, adding these rule types.

## Bibliography

[bib-brill1992] *A simple rule-based part of speech tagger*. E. Brill.
1992. Association for Computational Linguistics.

\

* * * * *

^[[1](#idp1215152)]^A full description of the Brown tag set can be found
at:
[http://www.scs.leeds.ac.uk/ccalas/tagsets/brown.html](http://www.scs.leeds.ac.uk/ccalas/tagsets/brown.html)

^[[2](#idp1235440)]^This String representation is also used by **ghci**
to print the value of a TrainingInstance.

* * * * *

  --------------------------------------------- --------------------- -----------------------------------------
  [Prev](chap-ir.xhtml)                                               [Next](chap-reglang.xhtml)
  Chapter 6. Information retrieval (proposed)   [Home](index.xhtml)   Chapter 8. Regular languages (proposed)
  --------------------------------------------- --------------------- -----------------------------------------


