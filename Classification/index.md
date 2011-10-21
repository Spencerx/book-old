Chapter 5. Classification

[Prev](chap-similarity.xhtml)

[Next](chap-ir.xhtml)

* * * * *

## Chapter 5. Classification

**Table of Contents**

[5.1. Introduction](chap-classification.xhtml#chap-classification-intro)

[5.2. Naive Bayes classification](chap-classification.xhtml#idp1136832)

[5.3. Maximum entropy
classification](chap-classification.xhtml#idp1139760)

## 5.1. Introduction

Many natural language processing tasks require classification, you want
to find out to which class a particular instance belongs. To make this
more concrete, we give three examples:

-   **Authorship attribution**: suppose that you were given a text, and
    have to pick the correct author of the text from three proposed
    authors.

-   **Part of speech tagging**: in part of speech tagging, words are
    classified morphosyntactically. For instance, we could classify the
    word 'loves' in the statement "John loves Mary" to be a verb.

-   **Fluency ranking**: in natural language generation, we want to find
    out whether a sentence produced by a generation system is fluent or
    not fluent.

Such classifications can be made based on specific characteristics of
the instance that we want to specify. These characteristics are called
features in natural language processing jargon. Suppose that you were
asked to determine the author of a text, and know that Jack tends to
write short sentences, while Steven and Marie tend to write long
sentences. Now, if you were given a text with mainly short sentences,
who would you attribute the text to? Probably Jack, right? Average
sentence length is one possible feature to classify the text by its
author.

More formally speaking, we want to estimate p(y|x) , the probability of
an event (classification), given a context. For instance, in authorship
attribution, the classification being a specific author is an event,
while the text is the context. In part of speech tagging, the
classification of a word as verb is an event, the word and surrounding
words are the context.

In this chapter, we will look at linear classifiers. A linear
classification can make a classification based on a linear combination
of features. To give an example, consider [Figure 5.1, “Linear and
non-linear
classifiers”](chap-classification.xhtml#fig-linear-nonlinear-classifier).
Here we see two classes of objects, that can be separated using just two
features (f1 and f2). One class is tends to have high f1 values, the
other high f2 values. The figure also shows two classifiers, c1 and c2,
that successfully separate both classes. c1 is a linear classifier, as
it is a linear combination of f1 and f2. c2, on the other hand, is not a
linear classifier: the effect of f1 becomes weaker as f2 increases.

**Figure 5.1. Linear and non-linear classifiers**

  -----------------------------------------------------------------------
  ![Linear and non-linear classifiers](../images/linear-classifier.svg)
  -----------------------------------------------------------------------

\

How do we find such functions? Doing it manually is not practical -
realistic models for natural language processing classification use
thousands to millions of features. Finding such functions is an art in
itself, and is usually called machine learning. Machine learning methods
learn such classifiers through training material. Machine learning
methods are a topic by themselves, so in this chapter we will mainly
look at the application of classifiers obtained through machine
learning.

This may all seem somewhat abstract at this point, but things will get
clearer as we dive into real classifiers. The thing to remember now is
that we want to attach a particular class label to instances, based on
features of that instance, and we will do this using linear classifiers.

## 5.2. Naive Bayes classification

Stub

## 5.3. Maximum entropy classification

### 5.3.1. Introduction

An important disadvantage of naive Bayes modelling is that it has strong
feature independence assumptions. Often, it is not clear whether
features are dependent, or you simply do not want to care. For instance,
coming back to the task of authorship attribution. Suppose that you made
a model that uses the average sentence length as a feature amongst
others. Now you got a fantastic idea, you want to add some features
modeling syntactic complexity of sentences in a text. Such features may
add new cues to the model, but syntactic complexity also has a
correlation with sentence length. In such situations, naive Bayes models
may fail, since they see these features as independent contributors to a
classification.

One class of models that do not assume independent features are maximum
entropy models. We can almost hear you think "isn't classification
supposed to minimize uncertainty"? That's a very good question, that we
will come to in a moment. First, we have to ask a basic question: given
a collection of training instances, what is a good model? Think about
this for a moment, without diving into theory and technicalities.

The answer is pretty simple: since the training data is (supposed to be)
a representative sample of reality, a good model would predict the
training data. What does it mean to predict the training data? You may
remember from high school math that you could calculate the expected
value of a random variable given a probability distribution. For
instance, if you play a coin tossing game, you can calculate the
(average) profit or loss given enough tosses. Suppose that a friend has
a biased coin, with p(heads) = 0.7 and p(tails) = 0.3. Winning gives you
Euro 1.50, when losing, you pay 1 Euro. The expected outcome of choosing
tails is 0.7 \* -1 + 0.3 \* 1.50 ≅ -0.25. Not such a good bet, huh?

If we know the expected value from the observation of repeated coin
flips (the training data), we can make a model that gives the same
outcome. If we know the payments, finding the model analytically is
trivial. What if we do the same for features? We can calculate the
feature value in the training data:

**Equation 5.1. Calculating the empirical value of a feature**

E p̃ [ f i ] = ∑ x,y p̃(x,y) fi (x,y)

\

It's easier than it looks: the empirical value of a feature f~i~ is the
sum of the multiplication joint probability of a context and an event in
the training data and the value of f~i~ for that context and event. We
can also calculate the expected value of a feature f~i~ according to the
conditional model p(y|x):

**Equation 5.2. Calculating the expected value of a feature**

E p [ f i ] = ∑ x,y p̃(x) p(y|x) fi (x,y)

\

Since p(x,y) ≡ p(x) p(y|x) , and the model only estimates the
conditional probability p(y|x), the probability of the context in the
training data, p̃(x) , is used. To make the model predict the training
data, a constraint is added for each feature f~i~ during the training of
the model, such that:

**Equation 5.3. Constraining the expected value to the empirical value**

E p̃ [ f i ] = E p [ f i ]

\

For any non-trivial model, the model that satisfies these constraints
cannot be found analytically. In fact, there is normally even an
infinite number of models. So, then the question becomes, which model do
we use? Consider, for example, the classifiers in [Figure 5.2, “Two
competing models”](chap-classification.xhtml#fig-competing-classifiers).
While both classifiers separate instances of both classes neatly, c2 is
the better classifier; it separates the classes with a wider margin than
c1, and as such has more tolerance with respect to unseen instances that
fall outside the current class boundaries. For instance, if an instance
has a high value for f1, and a slightly higher value for f2, c1
attribute this instance to the other class while there is no reason to
believe that this is true. Or in other words, c1 has a bias.

**Figure 5.2. Two competing models**

![Two competing models](../images/classifier-quality.svg)

\

Now the idea is clear: we want a model that satisfies a set of
constraints, but also has as few assumptions as possible.

Let's forget those constraints for a moment and get back to something
simple, like coin flipping. We have two possible outcomes, head and
tail. If we have no assumptions about the coin being biased and such, we
(should) believe that the probability of getting head or tail is
half-half. And what if we model dice roles? If we believe that the
(cube) dice is not biased and no trickery is involved, the probability
of each outcome should be 1/6th. In both cases, the outcomes have a
uniform distribution, meaning that every outcome is equally probable. In
the uniform distribution, the probability of a particular outcome is 1
|O| , where |O| is the number of possible outcomes. In the uniform
distribution, uncertainty is at its maximum. If we know that p(tails) =
0.9, we know pretty certain that a coin flip will result in tails.
However, if p(tails) = 0.5, we are uncertain about the outcome. In fact,
there is no possible distribution that has a higher uncertainty. A
measure of uncertainty is entropy.

So our model should be uniform as possible, while still obeying the
constraints that are imposed. We can find the most uniform model by
maximizing entropy.

More to be done...

* * * * *

  ----------------------------------------------- --------------------- ---------------------------------------------
  [Prev](chap-similarity.xhtml)                                         [Next](chap-ir.xhtml)
  Chapter 4. Distance and similarity (proposed)   [Home](index.xhtml)   Chapter 6. Information retrieval (proposed)
  ----------------------------------------------- --------------------- ---------------------------------------------


