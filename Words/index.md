---
title: Words
---

# Words

##  Introduction

Words are the most fundamental building blocks of our language. Although
they may look simple on the surface, they are very ingenious devices
that pack not only meaning, but also grammatical information. For our
purposes, we will say that a word consists of a *stem* and *affixes*.
Let's look at three simple sentences:

-   I **walk**.

-   John **walk**s.

-   Jack **walk**ed.

All three sentences contain some 'form' of *walk*. We say that these
instances are all *inflections* of the verb walk. The part of the
inflections that is shared (*walk*) is what we call the *stem*. The
parts that are not common are named *affixes*. We inflect verbs to
indicate tense (present, past, etc.), the person of the verb's subject
(first, second, and third), and the number (singular or plural). The
affix *s* in John walk*s*, for instance, tells us (in combination with
the subject John) that the verb *walk* is in present tense, third person
singular.

Other types of words have inflections as well. For example, we inflect
nouns to distinguish singular and plural:

I saw one **duck**. I saw two **duck**s.

Up to this point, we have just seen one kind of affix: one that is glued
to the end of the word. There are actually many types of affixes. For
now, you should only know about two:

-   Prefix: appears in front of the stem. For example, **un**believable.

-   Suffix: appears after the stem. For example, duck**s**.

Now, with that out of the way, let's get some work done.

##  Playing with words

Written words consist of characters. We can write down characters in
Haskell with single quotes. If you type in a character in *ghci*, it
will simply echo back the character:

~~~~ {.haskell}
Prelude> 'h'
'h'
~~~~

This is all that *ghci* does, it evaluates whatever you type. A
character evaluates to... a character. We confirm that Haskell agrees
with us that this actually a character by asking the type with *:type*
or its shorthand *:t*:

~~~~ {.haskell}
Prelude> :type 'h'
'h' :: Char    
~~~~

Great. Haskell indeed confirms that 'h' is a character, or in Haskell's
words: that 'h' is of type *Char*. Not all that practical with the small
amount of single-lettered words in English though. Rather than a single
character, we want a sequence of characters. Not surprisingly, Haskell
has a data types to build sequences. The most commonly used sequence is
the list. You can have lists of many things: lists of groceries, lists
of planets, but also lists of characters. We can make a literal list in
Haskell by enumerating its elements, separated by commas and surrounded
by square brackets. For instance, the list *1, 2, 3, 4, 5* is written as
*[1, 2, 3, 4, 5]*. Let's try to make a list of characters:

~~~~ {.haskell}
Prelude> ['h','e','l','l','o']
"hello" 
~~~~

Now we are getting somewhere! Let's look at the type of this list:

~~~~ {.haskell}
Prelude> :type ['h','e','l','l','o']
['h','e','l','l','o'] :: [Char] 
~~~~

Its type is *[Char]*, which should be read as 'list of characters'. Such
a list of characters is known as *a string* Of course, writing down
words in this manner is not very convenient. Fortunately, as the
evaluation of the second to last example already suggests, there is a
more convenient notation. We can represent strings by wrapping
characters in double quotes:

~~~~ {.haskell}
Prelude> "hello"
"hello"
Prelude> :type "hello"
"hello" :: [Char] 
~~~~

We will take this opportunity to seriously demolish some words, but all
with the noble cause of learning some commonly-used Haskell list
functions. The first function `length`{.function} returns the length of
a list:

~~~~ {.haskell}
Prelude> length "hello"
5
Prelude> length [1,2,3]
3 
~~~~

To get a better impression of functions, it is often useful to look at
its type:

~~~~ {.haskell}
Prelude> :type length
length :: [a] -> Int 
~~~~

That's one heck of a type! Basically, it says 'give me a list of
something (denoted by the *a* between the list brackets), then I will
give you an Int'. In these so-called *type signatures*, letters that are
not capitalized are generic, meaning that they can be of some
unspecified type. That is, *[a]* is a list with elements of some type.
**But:** all elements should be of the same type. An *Int* is an
integral number: a positive or negative whole number.

Two other basic list functions are `head`{.function} and
`tail`{.function}. `head`{.function} returns the first element of a
list, `tail`{.function} everything but the first element:

~~~~ {.haskell}
Prelude> head "hello"
'h'
Prelude> tail "hello"
"ello"
~~~~

The type of head is the following:

~~~~ {.haskell}
Prelude> :type head
head :: [a] -> a
            
~~~~

Hey, two *a*'s! Equipped with the knowledge we have, we know that
`head`{.function} is a function that takes a list of something, and
gives back something. But there is an additional constraint here:
although *a* is some type, all *a*'s have to be the same type. So,
applying `head`{.function} to a list of numbers gives a number, applying
`head`{.function} to a list of characters gives a character, etc.

In analogy, the type of `tail`{.function} should now be easy to
understand:

~~~~ {.haskell}
Prelude> :type tail
tail :: [a] -> [a] 
~~~~

We apply `tail`{.function} to a list of some type, and get back a list
with the same type.

Finally, the last function for now is `reverse`{.function}. We have to
admit presenting this function with a bit of joy, since it will allow us
to write our first little useful Haskell program. As expected,
`reverse`{.function} reverses the elements of a list:

~~~~ {.haskell}
Prelude> reverse "hello"
"olleh"
~~~~

Olé! And another one:

~~~~ {.haskell}
Prelude> reverse "level"
"level"
~~~~

Hold on there! We bumped into a palindrome: a word that is read the same
way, no matter whether it is read forward or backward. Now, suppose we
would like to write our own function to determine whether a word is a
palindrome. We first need to make a slightly more formal definition of a
palindrome: a word is a palindrome if it is equal to its reverse. In
Haskell we can compare values using the `==`{.function} operator:

~~~~ {.haskell}
Prelude> "hello" == "hello"
True
Prelude> "hello" == "olleh"
False 
~~~~

Such a comparison evaluates to *True* if both values are equal, or to
*False* in case they are not. *True* and *False* are the only values of
the *Bool* type. Since `reverse`{.function} also returns a value,
nothing holds us from using it in comparisons:

~~~~ {.haskell}
Prelude> "hello" == reverse "hello"
False
Prelude> "level" == reverse "level"
True 
~~~~

The test that we devised for detecting palindromes seems to work. But it
is a lot of typing. Luckily, we can generalize this into a function.
Let's replace both words by the symbolic name *word* (but don't execute
this in **ghci** yet, since it does not know this symbolic name):

~~~~ {.haskell}
word == reverse word 
~~~~

And as a next step, Let's do some magic:

~~~~ {.haskell}
Prelude> let palindrome word = word == reverse word 
~~~~

This defines the function `palindrome`{.function} taking one argument,
and binds this argument to the symbolic name *word*. To this function we
assign the expression *word == reverse word*. Play a little with this
function to be convinced that it actually works. Some examples:

~~~~ {.haskell}
Prelude> palindrome "hello"
False
Prelude> palindrome "level"
True
Prelude> palindrome "racecar"
True 
~~~~

If this function is still a mystery to you, it may be useful to write
down the application of the function stepwise for a word that is not a
palindrome:

~~~~ {.haskell}
palindrome "hello"
palindrome "hello" = "hello" == reverse "hello"
palindrome "hello" = "hello" == "olleh"
palindrome "hello" = False     
~~~~

and a word that *is* a palindrome:

~~~~ {.haskell}
palindrome "racecar"
palindrome "racecar" = "racecar" == reverse "racecar"
palindrome "racecar" = "racecar" == "racecar"
palindrome "racecar" = True
~~~~

Congratulations, you have made your first function, which is in essence
a small program!

##  From words to sentences

So far, we have looked at words in isolation. However, in language,
words are often combined to form a higher level of meaning
representation: a sentence. Provided what we have learned about
representing words in Haskell, the step towards representing sentences
should be a minor one. We could, for example, represent sentences in the
exactly the same way we represented words:

~~~~ {.haskell}
Prelude> "The cat is on the mat."
"The cat is on the mat." 
~~~~

That's fine for a beginning, although not so convenient. Let us see why.
Assume we ask you to give us the first word of a sentence. In the
previous section, we learned that `head`{.function} can be used to get
the first element of a list. Let's try to apply that here:

~~~~ {.haskell}
Prelude> head "The cat is on the mat."
'T' 
~~~~

As you probably expected, that didn't work. We represented a sentence as
a list of characters (a string), and hence asking for the first element
will give the first character. But wait! What if we represented a
sentence as a list of words?

~~~~ {.haskell}
Prelude> ["The", "cat", "is", "on", "the", "mat", "."]
["The","cat","is","on","the","mat","."]
Prelude> :type ["The", "cat", "is", "on", "the", "mat", "."]
["The", "cat", "is", "on", "the", "mat", "."] :: [[Char]]
~~~~

Nifty! We just constructed a list, of a list of characters. Though, you
may wonder why we made the punctuation at the end of the sentence a
separate "word". Well, this is mostly a pragmatic choice, because gluing
this punctuation sign to *mat* does not really form a word either.
Having the period sign separate is more practical for future processing.
Hence, formally we say that a sentence consists of tokens, where a token
can be a word, a number, and a punctuation sign.

Rinse and repeat:

~~~~ {.haskell}
Prelude> head ["The", "cat", "is", "on", "the", "mat", "."]
"The"
~~~~

Since a word is also a list, we can apply a function to words as well.
For example, we can get the first character of the first word by
applying `head`{.function}, to the `head`{.function} of a sentence:

~~~~ {.haskell}
Prelude> head (head ["The", "cat", "is", "on", "the", "mat", "."])
'T'
~~~~

Note that we need parenthesis to force Haskell to evaluate the part in
parentheses first. If we do not enforce this order of evaluation,
Haskell will try to evaluate *head head* first, which makes no sense.
Remember that `head`{.function} requires a list as its argument, and
`head`{.function} is not a list.

Now that we know how to represent sentence, this is a good time to try
to write yet another small program. This time, we will write a function
to compute the average token length in a corpus (a collection of texts).
Since we did not look at real corpora yet, pick any sentence you like as
*My Little Corpus*™. The authors will use *"Oh, no, flying pink
ponies!"* The average token length is the sum of the lengths of all
tokens, divided by the total number of tokens in the corpus. So,
stepwise, we have to:

1.  Get the length of each token in the corpus.

2.  Sum the lengths of the tokens.

3.  Divide the sum by the length of the corpus.

You know how to get the in characters length of a single token:

~~~~ {.haskell}
Prelude> length "flying"
6
~~~~

Since you are lazy, you are not going to apply `length`{.function} to
every token in the corpus manually. Instead we want tell Haskell "Hey
Haskell! Please apply this length function to each element of the list."
It turns out that Haskell has a function to do this which is called
`map`{.function}. Time to inspect `map`{.function}:

~~~~ {.haskell}
Prelude> :type map
map :: (a -> b) -> [a] -> [b]
~~~~

And we are in for another surprise. The most surprising element is
probably the first element in the type signature, *(a -\> b)*. Also
surprising is that we now see three types, *(a -\> b)*, *[a]* and *[b]*.
The latter is simple: this function takes two arguments, *(a -\> b)* and
*[a]*, and returns *[b]*. *(a -\> b)* as the notation suggests, is a
function taking an *a* and returning a *b*. So, `map`{.function} is
actually a function that takes a function as its argument, or in
functional programming-speak: a *higher order* function.

So, `map`{.function} is a function that takes a function that maps from
*a* to *b*, takes a list of *a*s, and returns a list of *b*s. That looks
a suspicious lot like what we want! We have a list of tokens represented
as strings, the function length that takes a list and returns its length
as an integer, and we want to have a list of integers representing the
lengths. Looks like we have a winner!

~~~~ {.haskell}
Prelude> map length ["Oh", ",", "no", ",", "flying", ",", "pink", "ponies","!"]
[2,1,2,1,6,1,4,6,1]
~~~~

We have now completed our first step: we have the length of each token
in the corpus. Next, we have to sum the lengths that we have just
retrieved. Fortunately, Haskell has a `sum`{.function} function:

~~~~ {.haskell}
Prelude> :type sum
sum :: (Num a) => [a] -> a
~~~~

This function takes a list of *a*s, and returns an *a*. But where did
the *(Num a) =\>* come from? Well, *Num* is a so-called typeclass. A
type can belong to one or more of such typeclasses. But belonging to a
typeclass does not come without cost. In fact, it requires that certain
functions need to be defined for types that belong to it. For instance,
the typeclass *Num* is a typeclass for numbers, which requires amongst
others, functions that define addition or subtraction. Coming back to
the type signature, `sum`{.function} will sum a list of *a*s, but not
just any *a*s, only those that belong to the typeclass *Num*. And after
all, this makes sense, doesn't it? We cannot sum strings or planets, but
we can sum numbers. In fact, we can only sum numbers.

After this solemn introduction into typeclasses, feel free to take a cup
of tea (or coffee), and try step two:

~~~~ {.haskell}
Prelude> :{
sum (map length ["Oh", ",", "no", ",", "flying", ",", "pink", "ponies", "!"])
:}
24
~~~~

By now, you will probably smell victory. The only step that remains is
to divide the sum by the length of the sentence using the division
operator (*/*):

~~~~ {.haskell}
Prelude> :{
sum (map length ["Oh", ",", "no", ",", "flying", ",", "pink", "ponies", "!"]) /
  length ["Oh", ",", "no", ",", "flying", ",", "pink", "ponies", "!"]
:}

<interactive>:1:0:
    No instance for (Fractional Int)
      arising from a use of `/' at <interactive>:1:0-136
    Possible fix: add an instance declaration for (Fractional Int)
    In the expression:
          sum (map length ["Oh", ",", "no", ",", ....])
        / length ["Oh", ",", "no", ",", ....]
    In the definition of `it':
        it = sum (map length ["Oh", ",", "no", ....])
           / length ["Oh", ",", "no", ....]
~~~~

And we have... Failure! I hope you poured yourself a cup of herb tea!
(again alternatively: espresso!) While this is all a bit cryptic, the
second line (*No instance for (Fractional Int)*) gives some idea where
the trouble stems from. *Fractional* is typeclass for fractional
numbers, and Haskell complains that Int is not defined to be of the
typeclass *Fractional*. This sounds obvious, since an integer is not a
fractional number. In other words, Haskell is trying to tell us that
there is an *Int* in some place where it expected a type belonging to
the typeclass *Fractional*. Since the division is the only new
component, it is the first suspect of the breakdown:

~~~~ {.haskell}
Prelude> :type (/)
(/) :: (Fractional a) => a -> a -> a
~~~~

First off, notice that we have put the division operator in parentheses.
We have done this because the division operator is used as a so-called
*infix function*: it is a function that is put between its arguments
(like *1.0 / 2.0*). By putting an infix operator in parentheses, you are
stating that you would like to use it as a regular function. This means
you can do things like this:

~~~~ {.haskell}
Prelude> (/) 1.0 2.0
0.5
~~~~

Anyway, the verdict of the type signature of `(/)`{.function} is clear,
it requires two arguments that belong to the *Fractional* typeclass. The
sum and length that we calculated clearly do not belong to this
typeclass, since they are of the type *Int*:

~~~~ {.haskell}
Prelude> :{
:type
  sum (map length ["Oh", ",", "no", ",", "flying", ",", "pink", "ponies", "!"])
:}
sum (map length ["Oh", ",", "no", ",", "flying", ",", "pink", "ponies", "!"])
  :: Int
Prelude> :{
:type
  length ["Oh", ",", "no", ",", "flying", ",", "pink", "ponies", "!"]
:}
length ["Oh", ",", "no", ",", "flying", ",", "pink", "ponies", "!"]
  :: Int
~~~~

Fortunately, Haskell provides the function `fromIntegral`{.function}
that converts an integer to any kind of number. Add
`fromIntegral`{.function}, and you surely do get the average token
length of the corpus:

~~~~ {.haskell}
Prelude> :{
fromIntegral
  (sum (map length ["Oh", ",", "no", ",", "flying", ",", "pink", "ponies", "!"])) /
  fromIntegral (length ["Oh", ",", "no", ",", "flying", ",", "pink", "ponies", "!"])
:}
2.6666666666666665
~~~~

Well, that was a bumpier ride than you might have expected. Don't worry!
During our first forays into Haskell, we were convinced that we were too
stupid for this too (and here we are writing a book). However, after
more practice, you will learn that Haskell is actually a very simple and
logical language.

Maybe it will feel more like a victory after generalizing this to a
function. You can follow the same pattern as in the palindrome example:
replace the sentence with a symbolic name and transform it into a
function:

~~~~ {.haskell}
Prelude> :{
let averageLength l =
  fromIntegral (sum (map length l)) / fromIntegral (length l)
:}
Prelude> :{
averageLength ["Oh", ",", "no", ",", "flying", ",", "pink" ,"ponies", "!"]
:}
2.6666666666666665
~~~~

Congratulations, you just wrote your second function! But wait, you
actually accomplished more than you may expect. Check the type signature
of `averageLength`{.function}.

~~~~ {.haskell}
Prelude> :type averageLength
averageLength :: (Fractional b) => [[a]] -> b
~~~~

You made your first weird type signature. Show it off to your
colleagues, significant other, or dog. `averageLength`{.function} is a
function that takes a list of a list of *a*, and returns a *b* that
belongs to the *Fractional* typeclass. But wait, *a* can be anything,
right? What happens if we apply this function to a list of sentences?

~~~~ {.haskell}
Prelude> averageLength [["I", "like", "Haskell", "."],
  ["Ruby", "rocks", "too", "."],
  ["Who", "needs", "Java", "?"]]
4.0
~~~~

Woo! That's the average sentence length, expressed in number of words.
It turns out that, although we set out to make a function to calculate
the average token length, we wrote a function that calculates the
average length of lists in a list (e.g., characters in words, words in
sentences, or sentences in a text). This happens very often when you
write Haskell programs: lots of functions are generic and can be reused
for other tasks.

##  A note on tokenization

When dealing with real-world text, it is usually not neatly split in
sentences and tokens. For example, consider this book - punctuation is
usually glued to words. These processes, sentence splitting and
tokenization may seem trivial, unfortunately they are not. Consider the
following sentence:

E.g. Jack doesn't have 19.99 to spend.

If we simply perform sentence splitting on periods (*.*), we will find
four sentences:

1.  *E.*

2.  *g.*

3.  *Jack doesn't have 19.*

4.  *99 to spend.*

Of course, it is just one sentence. Similar problems arise during
punctuation: how do we know that *E.g.* and *19.99* should not be split?
And how about *doesn't*, which should probably be split as *does n't* or
*does not*? Tokenization can be performed accurately, but it requires
techniques that you will see in later chapters. So don't worry, we will
get back to proper tokenization later on. We promise!

Of course, up to the point where we handle tokenization, we need
material to work on. To make life easier for you, the material for the
first chapters of the book is pre-tokenized in a plain-text file using
two simple rules:

1.  One sentence per line.

2.  Tokens are separated by a space.

To convert a text file to a Haskell representation, sentence splitting
is a matter of splitting by line, and tokenization a matter of splitting
by space. Have a look at the following example:

~~~~ {.haskell}
Prelude> "This is Jack .\nHe is a Haskeller ."
"This is Jack .\nHe is a Haskeller ."
~~~~

This is exactly the representation that we will be using for our textual
data. As you can see, the tokens are separated by spaces. Both sentences
are separated using a newline. When writing down a string literally, you
can insert a newline using \\n.

Haskell provides a `lines`{.function} function to split up a string by
line. Not surprisingly, this function accepts a string as its first
argument, and will return a list of strings:

~~~~ {.haskell}
Prelude> :type lines
lines :: String -> [String]
Prelude> lines "This is Jack .\nHe is a Haskeller ."
["This is Jack .","He is a Haskeller ."]
~~~~

That was easy! Now to the actual tokenization. For all sentences, we
have a string representing the sentence. We want to split this string on
the space character. Haskell also has a function to do this, named
`words`{.function}. `words`{.function} is nearly the same function as
`lines`{.function}, except that it splits on spaces rather than
newlines:

~~~~ {.haskell}
Prelude> words "This is Jack ."
["This","is","Jack","."]
~~~~

That will do, but we have to apply this to every sentence in the list of
sentences. Recall that we can use the `map`{.function} function we have
seen earlier to apply the `words`{.function} function to each element of
the list of (untokenized) sentences:

~~~~ {.haskell}
Prelude> map words (lines "This is Jack .\nHe is a Haskeller .")
[["This","is","Jack","."],["He","is","a","Haskeller","."]]
~~~~

Allright! That will do the job. We know how to turn this into a
full-fledged function:

~~~~ {.haskell}
Prelude> let splitTokenize text = map words (lines text)
Prelude> splitTokenize "This is Jack .\nHe is a Haskeller ."
[["This","is","Jack","."],["He","is","a","Haskeller","."]]
~~~~

This is a good moment to beautify this function a bit. To make it
simpler, we first need to get rid of the parentheses. We used the
parentheses to tell Haskell that it should evaluate *lines text* first,
because it would otherwise try to map over the function
`lines`{.function}, which would fail, because it is not a list. Very
often, you will encounter function applications of the form *f(g(x))*,
or *f(g(h(x)))*, etc. Haskell provides the *(.)* function to combine
such function applications. So, *f(g(x))* can be rewritten to *(f . g)
x* (apply function *f* to the outcome of *g(x)*) and *f(g(h(x)))* as *(f
. g . h) x* (apply function *f* to the outcome of a function *g*, which
is in turn applied to the outcome of *h(x))*. As you can see, this
so-called function composition makes things much easier to read. We can
now rewrite our tokenization function by using function composition:

~~~~ {.haskell}
Prelude> let splitTokenize text = (map words . lines) text
~~~~

This states that we apply *map words* to the outcome *lines text*. This
may not yet seem so interesting. However, it allows us to make yet
another simplification step. Consider the type of the `map`{.function}
function:

~~~~ {.haskell}
Prelude> :type map
map :: (a -> b) -> [a] -> [b]
~~~~

`map`{.function} takes a function, and a list, and returns a list. Now
we will do something that may look weird, but is very common in
functional programming.

~~~~ {.haskell}
Prelude> :type map words
map words :: [String] -> [[String]]
~~~~

Applying `map`{.function} to just one argument will give... another
function! In fact, what we just did is bind only the first argument of
the map function, leaving the second unbound. This gives a novel map
function that only takes one argument, as it has its first argument
implicitly bound to the function words. This process is called
*currying* (indeed, named after the mathematician Haskell Curry) in
functional programming slang.

If we look back at our `splitTokenize`{.function} function, and look up
the type of *map words . lines*, we see that it is a function that takes
a *String* and returns a list of a list of strings:

~~~~ {.haskell}
Prelude> :type map words . lines
map words . lines :: String -> [[String]]
~~~~

In our function body, we apply this function to the argument *text*. Of
course, this is not really necessary, because *map words . lines*
already defines our function (as we have shown above). We just need to
bind this to the name `splitTokenize`{.function}. Consequently the
function can once more be simplified:

~~~~ {.haskell}
Prelude> let splitTokenize = map words . lines
splitTokenize :: String -> [[String]]
Prelude> splitTokenize "This is Jack .\nHe is a Haskeller ."
[["This","is","Jack","."],["He","is","a","Haskeller","."]]
~~~~

##  Word lists

In the following two sections, we will introduce two prototypical tasks
related to words. The first is to make a word (or actually token) list,
the second task is making a word frequency list.

A word list is a very simple data structure: it is just a list of
*unique* words or tokens that occur in a text. Our corpus is also just a
list of words, but since it contains duplicates, it is not a word list.
The obvious method to make a word list is to go through a corpus word by
word, and adding words that we have not yet seen to a second list. This
requires some functions we haven't seen yet:

-   Adding an element to a list.

-   Checking whether an element is (or is not) in a list.

-   Constructing a list while traversing another list.

We like easy things first, so let's start with the first item: adding an
element to a list. We have seen the `head`{.function} function before
that chops of the head of the list and returns it. But we can also do
the reverse: take a list and give it a new head. The old head then
becomes the head of the tail (are you still following?). In Haskell, we
can do this using the `(:)`{.function} function:

~~~~ {.haskell}
Prelude> 2 : [3,4,5]
[2,3,4,5] 
~~~~

Ain't that great? We can also add a head, and yet another:

~~~~ {.haskell}
Prelude> 1 : 2 : [3,4,5]
[1,2,3,4,5] 
~~~~

What if we do not have an element yet? Add the head to the empty list
(*[]*):

~~~~ {.haskell}
Prelude> "Hi" : []
["Hi"] 
~~~~

With that covered, the next thing we need to be able to do is checking
whether some element belongs to a list. We can do this using the
`elem`{.function} function. It takes an element as its first argument,
and a list as its second. It will return a Bool of the value *True* if
the element was in the list, or *False* otherwise. For example:

~~~~ {.haskell}
Prelude> elem 2 [1,2,3,4,5]
True
Prelude> elem 6 [1,2,3,4,5]
False 
~~~~

The function `notElem`{.function} is exactly the inverse of
`elem`{.function}, and returns *True* if an element is not in the list,
and *False* otherwise:

~~~~ {.haskell}
Prelude> notElem "foo" ["foo","bar","baz"]
False
Prelude> notElem "pony" ["foo","bar","baz"]
True
~~~~

Ok, so we want to add an element to a list if, but only if, it is true
that it is not yet a member of that list. Or in other words, the
addition is conditional. Haskell provides a set of keywords to model
conditionals, if..then..else. The structure is like this:

~~~~ {.haskell}
if expr then a else b 
~~~~

This whole structure itself is an expression. This expression evaluates
to *a* if *expr* evaluates to *True* or to *b* if *expr* evaluates to
False. To give a working, but useless example:

~~~~ {.haskell}
Prelude> if 1 == 2 then "cuckoo" else "egg"
"egg"
Prelude> if 1 == 1 then "cuckoo" else "egg"
"cuckoo"
~~~~

This looks exactly like what we need. Just fill in the blanks:

~~~~ {.haskell}
Prelude> if elem "foo" ["foo","bar","baz"] then ["foo","bar","baz"]
  else "foo" : ["foo", "bar", "baz"]
["foo","bar","baz"]
Prelude> if elem "pony" ["foo","bar","baz"] then ["foo","bar","baz"]
  else "pony" : ["foo", "bar", "baz"]
["pony","foo","bar","baz"]
~~~~

That's a bit contrived, but (as you hopefully see) not if we rewrite it
to a function:

~~~~ {.haskell}
Prelude> let elemOrAdd e l = if elem e l then l else e:l
Prelude> elemOrAdd "foo" ["foo", "bar", "baz"]
["foo","bar","baz"]
Prelude> elemOrAdd "pony" ["foo", "bar", "baz"]
["pony","foo","bar","baz"]
~~~~

Good, now we need to apply this to all words in a text, starting with an
empty list. Haskell provides a function to do this, but brace yourself,
the first time it may look a bit 'difficult'. It is named
`foldl`{.function} (a so-called) left fold. A left fold traverses a list
from head to tail, applying a function to each element, just like
`map`{.function}. However, the difference is that it can, but does not
necessarily return a list. As such, it is a generalization of the
`map`{.function} function. As usual, you can inspect the type signature
to see the arguments of `foldl`{.function}:

~~~~ {.haskell}
Prelude> :type foldl
foldl :: (a -> b -> a) -> a -> [b] -> a
~~~~

Now consider this example using `foldl`{.function}:

~~~~ {.haskell}
Prelude> foldl (+) 0 [1,2,3,4,5]
15
~~~~

Stepwise, this fold is executed in the following manner:

~~~~ {.haskell}
foldl (+) 0 [1,2,3,4,5]
foldl (+) ((0)+1) [2,3,4,5]
foldl (+) (((0)+1)+2) [3,4,5]
foldl (+) ((((0)+1)+2)+3) [4,5]
foldl (+) (((((0)+1)+2)+3)+4) [5]
foldl (+) ((((((0)+1)+2)+3)+4)+5)) []
((((((0)+1)+2)+3)+4)+5))
~~~~

So, it works by applying a function to some initial argument (*0* in
this case) as its first argument, and the first element of the list as
its second argument. When processing the second element of the list,
this expression is then the first argument of the function, and the
second element is the second argument, etc. The first argument of the
function that is applied is also called the *accumulator*, since it
accumulates results up till that point.

This could also work for our `elemOrAdd`{.function} function.
Unfortunately, `elemOrAdd`{.function} requires the accumulator as the
second argument, and the function passed to `foldl`{.function} as the
first argument. Compare the type signatures:

~~~~ {.haskell}
Prelude> :type foldl
foldl :: (a -> b -> a) -> a -> [b] -> a
Prelude> :type elemOrAdd
elemOrAdd :: (Eq a) => a -> [a] -> [a]
~~~~

In the function that is the first argument to `foldl`{.function}, the
return type is the same as the type of the first argument. In the case
of `elemOrAdd`{.function}, the type of the second argument corresponds
to that of the first. Of course, an easy 'hack' to solve this, is to
redefine elemOrAdd, switching its arguments, and plug it into foldl:

~~~~ {.haskell}
Prelude> let elemOrAdd l e = if elem e l then l else e:l
~~~~

Now, since we are building a list, we use the empty list (*[]*) as the
initial accumulator for this fold:

~~~~ {.haskell}
Prelude> foldl elemOrAdd [] ["blue", "blue", "red", "blue", "red"]
["red","blue"]
~~~~

That looks good! Stepwise, the fold works like this:

~~~~ {.haskell}
foldl elemOrAdd [] ["blue", "blue", "red", "blue", "red"]
foldl elemOrAdd ("blue":([])) ["blue", "blue", "red", "blue", "red"]
foldl elemOrAdd ("blue":([])) ["blue", "red", "blue", "red"]
foldl elemOrAdd ("blue":([])) ["red", "blue", "red"]
foldl elemOrAdd ("red":("blue":([]))) ["blue", "red"]
foldl elemOrAdd ("red":("blue":([]))) ["red"]
foldl elemOrAdd ("red":("blue":([]))) []
("red":("blue":([])))
["red","blue"]
~~~~

Now we wrap it up in another function, and you have constructed two
functions that, together, make word lists:

~~~~ {.haskell}
Prelude> let wordList = foldl elemOrAdd []
Prelude> wordList ["blue", "blue", "red", "blue", "red"]
["red","blue"]
~~~~

While our little word list function works fine on small texts, it will
not be very efficient for big corpora. The reason is simple - suppose
that we have already found 100,000 different tokens. For every word, it
would have to check the list of 100,000 tokens. There is no other way of
doing this than to traverse the list word by word. Or, on average, we
compare a token to 100,000 / 2 = 50,000 elements in the list. As a
computer scientist would say: `elemOrAdd`{.function} works in linear
time, its processing time is linear to the number of different tokens
that were seen.

This is a typical case of picking the wrong data structure for the task.
But for illustrative purposes, using lists was nice and simple. However,
since you are a working programmer, you want workable solutions. Bring
in the sets! A set is, like the mathematical set, a collection that does
not contain duplicate elements. That's good, because a word list does
not contain duplicate elements. Silly us, we were fooled by the word
*list*. What we actually want to build is a word set. In fact, it is
just for historical purposes that it is called a word list.

Beside the uniqueness of elements, another nice property of sets, as
they are normally implemented, is that set membership can be checked
rather quickly. In the sets that we will use, membership checking is in
logarithmic time. Or in other words, if comparison took one second, we
would on average need 50,000 seconds to search the list mentioned
earlier, but only *log(100,000)* or approximately 11.5 seconds to check
whether the element is in a set. Talking about optimizations!

Haskell provides set functionality, but not in the so-called *Prelude*.
*Prelude* is a module that contains functions. The *Prelude* module is
always loaded, so its functions are always available (unless you
explicitly ask Haskell to hide them). The functions `map`{.function},
`head`{.function}, `tail`{.function}, and `length`{.function}, for
instance, are defined in the *Prelude*. Functions for set manipulation,
on the other hand, are defined in a module named *Data.Set*. For the
time being, we will access functions from modules by prefixing the name
of the module. For instance, this will give us the empty set:

~~~~ {.haskell}
Prelude> Data.Set.empty
fromList []
~~~~

Like a list, a set can contain elements of various types. We see this
when inspecting the type signature of the `empty`{.function} function:

~~~~ {.haskell}
Prelude> :type Data.Set.empty
Data.Set.empty :: Data.Set.Set a
~~~~

`empty`{.function} returns a Set of some type *a*. We can also construct
a *Set* from a list using the `fromList`{.function} function:

~~~~ {.haskell}
Prelude> Data.Set.fromList [5,2,5,8,1,1,23]
fromList [1,2,5,8,23]
~~~~

As you can see here, the set does not contain duplicates. Another nice
property of Haskell sets is that they are ordered. We can also do the
inverse, convert a set to a list using `toList`{.function}:

~~~~ {.haskell}
Prelude> Data.Set.toList (Data.Set.fromList [5,2,5,8,1,1,23])
[1,2,5,8,23]
~~~~

Elements can be added to or removed from a *Set* using respectively the
`insert`{.function} and `delete`{.function} functions. Both functions
return a set with that element inserted or removed:

~~~~ {.haskell}
Prelude> Data.Set.insert 42 (Data.Set.fromList [5,2,5,8,1,1,23])
fromList [1,2,5,8,23,42]
Prelude> Data.Set.delete 5 (Data.Set.fromList [5,2,5,8,1,1,23])
fromList [1,2,8,23]
~~~~

Finally, we can check whether some value is a member of a set by using
the `member`{.function} function:

~~~~ {.haskell}
Prelude> Data.Set.member 23 (Data.Set.fromList [5,2,5,8,1,1,23])
True
Prelude> Data.Set.member 24 (Data.Set.fromList [5,2,5,8,1,1,23])
False
~~~~

We have now seen enough to change our word list function. Rather than
checking whether a value is in a list and adding it if not, we check
whether it is in a *Set* and add it in when it is not:

~~~~ {.haskell}
Prelude> :{
let elemOrAdd s e =
  if Data.Set.member e s then s else Data.Set.insert e s
:}
Prelude> elemOrAdd (Data.Set.fromList [5,2,5,8,1,1,23]) 24
fromList [1,2,5,8,23,24]
~~~~

That was simple. But it feels a weird, right? The most vital
characteristic of a set is that it never contains duplicate elements,
why do we need to check for duplicates? We don't. So, forget about
`elemOrAdd`{.function}, we will only use *Data.Set.insert* from this
point. Our objective now is to traverse a list of tokens, adding each
token to a set, starting with the empty set. Our first take is this:

~~~~ {.haskell}
Prelude> let wordSet = foldl Data.Set.insert Data.Set.empty
~~~~

However, this will not work. Remember that in the function we give to
`foldl`{.function}, the accumulator has to be the first argument (the
reason why we swapped the elements of our initial elemOrAdd function)?
We are accumulating a *Set*, but the set is the second argument to
*Data.Set.insert*. We will pull a little trick out of our hat.

~~~~ {.haskell}
Prelude> let wordSet = foldl (\s e -> Data.Set.insert e s) Data.Set.empty 
~~~~

You might be thinking "Oh, no, more syntax terror! Does it ever stop?"
Actually, *( e -\> Data.Set.insert e s)* is very familiar. You could see
it as an inline function. In functional programming jargon, this is
called a *lambda*. Check out the type signature of the lambda:

~~~~ {.haskell}
Prelude> :type (\s e -> Data.Set.insert e s)
(\s e -> Data.Set.insert e s)
  :: (Ord a) => Data.Set.Set a -> a -> Data.Set.Set a
~~~~

It is just a function, it takes a set of some type *a*, a value of type
*a*, and returns *a*. Additionally, a has the typeclass Ord, which means
that some comparison operators should be defined for a. The lambda has
two arguments that are bound to *s* and *e*. The function body comes
after the arrow. To emphasize that this is just a function, the
following functions are equivalent:

~~~~ {.haskell}
myFun = (\s e -> Data.Set.insert e s)
myFun s e = Data.Set.insert e ss 
~~~~

Back to our `wordSet`{.function} function. We used the lambda to swap
the arguments of *Data.Set.insert*. *Data.Set.insert* takes a value and
a set, our lambda takes a set and a value. The rest of the function
follows the same pattern as *wordList*, except that we start with an
empty set rather than an empty list. The function works as expected:

~~~~ {.haskell}
Prelude> wordSet ["blue", "blue", "red", "blue", "red"]
fromList ["blue","red"]
~~~~

You have done it! You are now not only able to make a function that
creates a word list, but also one that is performant.

### Exercises

1.  To measure the vocabulary of a writer, a so-called type-token ratio
    can be calculated. This is the number of distinct tokens occurring
    in a text (types) divided by the total number of tokens in that
    text.

    **Equation 2.1. Type-token ratio**

    tt-ratio = types tokens

    \

    For instance the phrase "to be or not to be" contains six tokens and
    four types (*to*, *be*, *or*, *not*). The type-token ratio of this
    phrase is *4 / 6 = 2 / 3*.

    Write a function that calculates the type-token ratio of a list of
    tokens. You can use the *Data.Set.size* function to get the number
    of elements in a set.

##  Storing functions in a file

Now that we are writing longer and longer functions, it becomes more
convenient to define functions in a file rather than the **ghci**
prompt. You can do this by creating a file using a plain-text editor
with the *.hs* extension. Functions can be written down in the same
manner as in *ghci*, but without the preceding *let* keyword. It is also
highly recommended to add a type signature before the function. Haskell
will check the function against the type signature, and report an error
if they do not correspond. This will help you catch incorrect function
definitions.

The `palindrome`{.function} function discussed earlier in this chapter
can be written to a file like this:

~~~~ {.haskell}
palindrome :: (Eq a) => [a] -> Bool
palindrome word = word == reverse word 
~~~~

If you saved this file as *example.hs*, you can load it in **ghci**
using the *:l* (shorthand for *:load*) command:

~~~~ {.haskell}
Prelude> :l example
[1 of 1] Compiling Main             ( example.hs, interpreted )
Ok, modules loaded: Main.
*Main> palindrome "racecar"
True 
~~~~

For code fragments that use a module other than the prelude, add an
import statement at the top of the file. For example, the
`wordSet`{.function} function from the previous section should be saved
to a text file in the following manner:

~~~~ {.programlisting}
import qualified Data.Set

wordSet :: Ord a => [a] -> Data.Set.Set a
wordSet = foldl (\s e -> Data.Set.insert e s) Data.Set.empty 
~~~~

From now on, we assume that examples are written to a text file, except
when the *Prelude\>* occurs in the example.

##  Word frequency lists

The word list function that we built in the previous section works is
useful for various tasks, like calculating the type-token ratio for a
text. For some other tasks this is not good enough - we want to be able
to find out how often a word was used. We can expand a word list with
frequencies to make a *word frequency list*.

To be able to store word frequencies, every word has to be associated
with an integer. We could store such an association as a tuple. A tuple
is a data type with a fixed number of elements and a fixed type for an
element. Examples of tuples are:

-   (1,2,3)

-   ("hello","world")

-   ("hello",1)

As you can see, they differ from lists in that they can have values of
different types as elements. However, if you inspect the type signatures
of these tuples, you will see that the length and type for each position
is fixed:

~~~~ {.haskell}
Prelude> :type (1,2,3)
(1,2,3) :: (Num t, Num t1, Num t2) => (t, t1, t2)
Prelude> :type ("hello","world")
("hello","world") :: ([Char], [Char])
Prelude> :type ("hello",1)
("hello",1) :: (Num t) => ([Char], t) 
~~~~

To store frequencies, we could use a list of tuples of the type
*[([Char], Int)]*. The phrase "to be or not to be" could be stored as

~~~~ {.haskell}
[("to",2),("be",2),("or",1),("not",1)]
~~~~

However, this would be even less efficient than using lists for
constructing word lists. First, like `elemOrAdd`{.function} we would
potentially have to search the complete list to locate a word. Second,
we would have to reconstruct the list up to the point of the element. In
the `elemOrAdd`{.function} function we could just give the list a new
head, but now we would have to replace the element to update the word
frequency and add all preceding list items again. Since Haskell is a
'pure' language, we cannot modify existing values.

A more appropriate data type for this task is a map (not to be confused
with the `map`{.function} function). A map maps a key to a value. In
Haskell, maps are provided in the *Data.Map* module. Like sets, we can
make an empty map:

~~~~ {.haskell}
Prelude> Data.Map.empty
fromList []
~~~~

When you inspect the type signature of the empty map, you can see that
it parametrizes over two types, a type for the key and a type for
values:

~~~~ {.haskell}
Prelude> :type Data.Map.empty
Data.Map.empty :: Data.Map.Map k a 
~~~~

We can construct a *Map* from a list of binary tuples (tuples with two
elements), where the first element of the tuple becomes the key, and the
second the value:

~~~~ {.haskell}
Prelude> Data.Map.fromList [("to",2),("be",2),("or",1),("not",1)]
fromList [("be",2),("not",1),("or",1),("to",2)]
Prelude> :type Data.Map.fromList [("to",2),("be",2),("or",1),("not",1)]
Data.Map.fromList [("to",2),("be",2),("or",1),("not",1)]
  :: (Num t) => Data.Map.Map [Char] t
~~~~

This also binds the types for the map: we are mapping from keys of type
string to values of type t that belongs to the *t* typeclass. No
specific value for types is used (yet), because the numbers could be
integers or fractionals.

The `insert`{.function} function is used to add a new mapping to the
*Map*:

~~~~ {.haskell}
Prelude> :{
  Data.Map.insert "hello" 1
    (Data.Map.fromList [("to",2),("be",2),("or",1),("not",1)])
:}
fromList [("be",2),("hello",1),("not",1),("or",1),("to",2)]
~~~~

If a mapping with the given key already exists, the existing mapping is
replaced:

~~~~ {.haskell}
Prelude> :{
  Data.Map.insert "be" 1
    (Data.Map.fromList [("to",2),("be",2),("or",1),("not",1)])
:}
fromList [("be",1),("not",1),("or",1),("to",2)]
~~~~

Looking up values is a bit peculiar. You can lookup a value with the
`lookup`{.function} function. However, if you inspect the type
signature, you will see that the value is not returned as is:

~~~~ {.haskell}
Prelude> :type Data.Map.lookup
Data.Map.lookup :: (Ord k) => k -> Data.Map.Map k a -> Maybe a
~~~~

Rather than returning a value, it returns the value packed in some box
called *Maybe*. *Maybe a* is a type that has just two possible so-called
*constructors*, *Just a* or *Nothing*. You can put your own values in a
*Maybe* box using the *Just a* constructor:

~~~~ {.haskell}
Prelude> Just 22
Just 22
Prelude> :type Just 22
Just 22 :: (Num t) => Maybe t
Prelude> Just [1,2,3,4,5]
Just [1,2,3,4,5]
Prelude> Just "stay calm"
Just "stay calm"
Prelude> :type Just "stay calm"
Just "stay calm" :: Maybe [Char]
~~~~

You can also make a box that contains vast emptiness with the *Nothing*
constructor:

~~~~ {.haskell}
Prelude> Nothing
Nothing
Prelude> :type Nothing
Nothing :: Maybe a 
~~~~

These boxes turn out to be pretty cool: you can use them to return
something or nothing from functions, without resorting to all kinds of
abominations as exceptions or null pointers (if you never heard of
exceptions or pointers, do not worry, you have a life full of bliss).
Since *Maybe* is so nice, the `lookup`{.function} function uses it. It
will return the value packed with in a *Just* constructor if the key
occurred in the map, or Nothing otherwise:

~~~~ {.haskell}
Prelude> :{
  Data.Map.lookup "to"
    (Data.Map.fromList [("to",2),("be",2),("or",1),("not",1)])
:}
Just 2
Prelude> :{
  Data.Map.lookup "wrong"
    (Data.Map.fromList [("to",2),("be",2),("or",1),("not",1)])
:}
Nothing 
~~~~

As for handling these values - we will come to that later. Mappings are
deleted from a *Map* by key with the `delete`{.function} function. If a
key did not occur in the *Map*, the original map is returned:

~~~~ {.haskell}
Prelude> :{
  Data.Map.delete "to"
    (Data.Map.fromList [("to",2),("be",2),("or",1),("not",1)])
:}
fromList [("be",2),("not",1),("or",1)]
Prelude> :{
  Data.Map.delete "wrong"
    (Data.Map.fromList [("to",2),("be",2),("or",1),("not",1)])
:}
fromList [("be",2),("not",1),("or",1),("to",2)]
~~~~

Finally, a *Map* can be converted to a list using the
`toList`{.function} function:

~~~~ {.haskell}
Prelude> :{
  Data.Map.toList
    (Data.Map.fromList [("to",2),("be",2),("or",1),("not",1)])
:}
[("be",2),("not",1),("or",1),("to",2)]
~~~~

Alright. Back to our task at hand: constructing a word frequency list.
As with word lists, we want to traverse a list of words, accumulating
data. So, the use of `foldl`{.function} is appropriate for this task.
During each folding step, we take the Map created in a previous step. We
then lookup the value for the current step in the Map. If it does not
exist, we add it to the Map giving it a frequency of one. Otherwise, we
want to increase the frequency by one. The `countElem`{.function}
function does this:

~~~~ {.programlisting}
import qualified Data.Map

countElem :: (Ord k) => Data.Map.Map k Int -> k -> Data.Map.Map k Int
countElem m e = case (Data.Map.lookup e m) of
                  Just v  -> Data.Map.insert e (v + 1) m
                  Nothing -> Data.Map.insert e 1 m
~~~~

This function introduces the case construct. Remember that
`lookup`{.function} uses the nifty Maybe data type? The case construct
allows us to select an expression based on a constructor. If
`lookup`{.function} returned a value using the Just constructor, the key
was in the Map. In this case, we bind the value to the name v and add a
new value for this key. This value is the old value for this key
incremented by one. If a value with the Nothing constructor was
returned, the key was not in the Map. So, we will add it, and give it a
(frequency) value of 1.

The `countElem`{.function} function works as intended:

~~~~ {.haskell}
*Main> foldl countElem Data.Map.empty ["to","be","or","not","to","be"]
fromList [("be",2),("not",1),("or",1),("to",2)]
~~~~

While this was a nice exercise, the Data.Map.insertWith function can
drastically shorten our function. This function uses an update function
to update a value, or a specified value if the key is not present in the
Map:

~~~~ {.haskell}
*Main> :t Data.Map.insertWith
Data.Map.insertWith
  :: Ord k =>
     (a -> a -> a) -> k -> a -> Data.Map.Map k a -> Data.Map.Map k a
~~~~

The update function gets the specified value as its first argument, and
the old value as its second argument. Using `insertWith`{.function}, we
can shorten our function to:

~~~~ {.programlisting}
countElem :: (Ord k) => Data.Map.Map k Int -> k -> Data.Map.Map k Int
countElem m e = Data.Map.insertWith (\n o -> n + o) e 1 m
~~~~

If an element was not seen in the Map yet, a frequency of 1 will be
inserted. If the element does occur as a key in the map, the lambda adds
one to the old frequency. With `countElem`{.function} in our reach, we
can define the `freqList`{.function} function:

~~~~ {.programlisting}
freqList :: (Ord k) => [k] -> Data.Map.Map k Int
freqList = foldl countElem Data.Map.empty
~~~~

##  Monads

In the next section you will see how to read real text corpora using the
so-called IO monad. Before diving into the IO monad, we will give a
short introduction to monads. In Haskell, it happens very often that you
want to perform a series of computations on values that are wrapped
using some type, such as Maybe or a list. For instance, suppose that you
have a Map that maps a customer name to a customer number, and yet
another Map that maps a customer number to a list of order numbers:

~~~~ {.haskell}
*Main> let customers = Data.Map.fromList [("Daniel de Kok", 1000),
  ("Harm Brouwer", 1001)]
*Main> let orders = Data.Map.fromList [(1001, [128])]
Prelude> Data.Map.lookup "Harm Brouwer" customers
Just 1001
Prelude> Data.Map.lookup 1001 orders
Just [128]
~~~~

Now, we want to write a function that extracts a list orders for a given
customer, wrapped in a Maybe, so that Nothing can be returned if the
customer is not in the list or if the customer had no orders. Your first
attempt will probably be along the following lines:

~~~~ {.programlisting}
lookupOrder0 :: Data.Map.Map String Integer ->
                Data.Map.Map Integer [Integer] ->
                String -> Maybe [Integer]
lookupOrder0 customers orders customer =
    case Data.Map.lookup customer customers of
      Nothing -> Nothing
      Just customerId -> Data.Map.lookup customerId orders
~~~~

This function works as intended:

~~~~ {.haskell}
*Main> lookupOrder0 customers orders "Jack Sparrow"
Nothing
*Main> lookupOrder0 customers orders "Daniel de Kok"
Nothing
*Main> lookupOrder0 customers orders "Harm Brouwer"
Just [128]
~~~~

But case constructs will quickly start to stack up when more functions
are called that return Maybe. For each Maybe we follow the same
procedure: if the value is Nothing we end the computation with that
value, if the value is Just x we call the next function that possibly
uses `x`{.varname} as an argument. This is where the Monad typeclass
kicks in: all data types that are of the Monad typeclass implement the
`(>>=)`{.function} function. This function joins computations resulting
in that data type according to some logic. For instance, the Maybe monad
combines expressions that return Maybe in such a way that if one
expression returns Nothing, the whole joined expression also returns
Nothing, just like our case construct in the example above.
Consequently, we could rewrite the example above as:

~~~~ {.programlisting}
lookupOrder1 :: Data.Map.Map String Integer ->
                Data.Map.Map Integer [Integer] ->
                String -> Maybe [Integer]
lookupOrder1 customers orders customer =
    Data.Map.lookup customer customers >>= (\m -> Data.Map.lookup m orders)
~~~~

That surely shortened the function! But what does it do? Well, the
function performs the following steps:

-   `Data.Map.lookup customer customers`{.function}: Lookup
    `customer`{.varname} in `customers`{.varname}.

-   `(>>=)`{.function}: If the expression on the left-hand returned
    Nothing, the value of the full expression, and consequently
    `lookupOrder1`{.function}, is Nothing. If the lookup returned a
    value of type Just Integer, extract the Integer from the Just
    constructor, and pass it as the argument of the next function.

-   `(\m -> Data.Map.lookup m orders)`{.function}: Lookup the supplied
    argument in the `orders`{.varname} Map. The result becomes the
    result of `lookupOrder1`{.function}. We had to use a lambda, to make
    the element to be looked up the first argument.

The type signature of (\>\>=) is also very illustrative:

~~~~ {.haskell}
*Main> :type (>>=)
(>>=) :: (Monad m) => m a -> (a -> m b) -> m b
~~~~

The (\>\>=) simply unwraps a value from the monad and feeds it to a
function that returns a value wrapped in the same monad. The
`(>>=)`{.function} function has the freedom to perform any operation
(including not calling (a -\> mb)), as long as it returns a value of
type m b.

The actual implementation of (\>\>=) for the Maybe monad is very simple:

~~~~ {.programlisting}
(Just x) >>= k = k x
Nothing  >>= _ = Nothing
~~~~

That's all! If the left-hand side expression evaluates to Just x, the
expression on the left hand side is evaluated with `x`{.varname} as its
argument. If the left-hand side evaluates to Nothing, the right-end side
is not evaluated, and the whole expression returns Nothing.

There is yet another function that is essential to monads named return,
it does nothing else than wrapping a value in that monad. For instance,
the `maybeBool`{.function} function wraps a Bool value in a Maybe monad:

~~~~ {.programlisting}
maybeBool :: Bool -> Maybe Bool
maybeBool = return
~~~~

This is `maybeBool`{.function} in action:

~~~~ {.haskell}
*Main> maybeBool True
Just True
*Main> maybeBool False
Just False
~~~~

The implementation of `return`{.function} for the Maybe monad is
trivial:

~~~~ {.programlisting}
return = Just
~~~~

Let's get back to our order lookup function. You may have noticed that
the `(>>=)`{.function} function is somewhat imperative in nature: it
chains a set of expressions, where the left-hand expression is evaluated
before the right-hand. Due to this nature, Haskell has a do notation
that resembles imperative programs. This is `lookupOrder1`{.function}
using the do-notation:

~~~~ {.programlisting}
lookupOrder2 :: Data.Map.Map String Integer ->
                Data.Map.Map Integer [Integer] ->
                String -> Maybe [Integer]
lookupOrder2 customers orders customer = do
    customerId <- Data.Map.lookup customer customers
    orders     <- Data.Map.lookup customerId orders
    return orders
~~~~

You can see that we added the do keyword to start the do-notation. Every
expression that follows can be seen as just one element in a sequence of
(\>\>=) expressions. Consequently, each line is governed by the 'laws'
of the monad. If the first lookup fails, no further evaluation is
performed, and `lookupOrder2`{.function} will return Nothing. Otherwise,
computation continues. The do-notation also allows the use of the
backward arrow (<-) this arrow extracts a value from the monad, and
binds it to a variable.

We can simplify lookupOrder2 further. The last lookup already returns a
value wrapped in the Maybe monad, there is no need to extract it using
<- and wrap it again with `return`{.function}:

~~~~ {.programlisting}
lookupOrder :: Data.Map.Map String Integer ->
               Data.Map.Map Integer [Integer] ->
               String -> Maybe [Integer]
lookupOrder customers orders customer = do
    customerId <- Data.Map.lookup customer customers
    Data.Map.lookup customerId orders
~~~~

##  Reading a text corpus

Up to this point we have been using very artificial text corpora. At
most a few sentences. But you are in it for the real deal, right? Lucky
you, we will use a real (like really real) corpus starting from this
very moment. Of course, just to test functions we will start with small
examples. But you will be able to apply your functions to real data.

So-called I/O (input/output) is a delicate matter in Haskell. The reason
being that, as a pure functional language, it is not possible to modify
expressions or values once they exist. This leads to an admirable
quality of Haskell: given the same input, a function will always return
the same output. Or in other words, a function does not have
side-effects. Unlike most other languages, there is no state in a
function that can change, so the output can also not change. If a
function always evaluates to the same value given the same input, how
can you have I/O? For example, suppose that we open a file, and use a
function read a byte. And then we read yet another byte. The second byte
may be a different one. The reading function has a side-effect: it
increases the position within the file.

The Haskell developers have, clever as they are, found a solution to get
Pandora's box into Haskell. I/O in Haskell is performed in the so-called
IO monad. The IO monad is not very different from the Maybe monad, it
implements the `(>>=)`{.function} and return functions as required for
monads. However, there is a subtle, but very important difference.
Remember that you can extract a value from a Maybe value using its Just
constructor?

~~~~ {.programlisting}
value = case someMaybe of
  Just x = x
~~~~

The IO monad does not have a public constructor, so there is no way to
pry a value out of an IO monad. If you read a file as a list of Char, it
resides in the IO monad. You can apply any list function to this list,
however, the result of the function that is applied to a list will also
have to reside in the IO monad. A value can never escape the IO monad.
And this is how impure I/O is possible in Haskell without sacrificing
purity of the language: impure data stays in the IO monad, and can never
escape.

Now on to some real work. As said, functions that do IO return a value
wrapped in IO. For instance, the `putStrLn`{.function} function returns
an empty tuple packed in the IO monad:

~~~~ {.haskell}
Prelude> :type putStrLn "hello world!"
putStrLn "hello world!" :: IO ()
~~~~

This is just a normal value, you can bind it to a name in **ghci**:

~~~~ {.haskell}
Prelude> let v = putStrLn "hello world!"
v :: IO ()
~~~~

However, if we evaluate the value in **ghci**, it will execute this I/O
action:

~~~~ {.haskell}
Prelude> v
hello world!
~~~~

Of course, the same thing happens if we evaluate `putStrLn`{.function}
directly:

~~~~ {.haskell}
Prelude> putStrLn "hello world!"
hello world!
~~~~

Now, let's get to the interesting part: reading a file. In the files
distributed with this book, you will find the file
`brown.txt`{.filename}. This file contains the Brown corpus, a corpus of
written text of various kinds. The Brown corpus is already tokenized,
which makes our life a bit easier. Ok, first we need to open the file
using the `IO.openFile`{.function} function. `openFile`{.function}
requires a filename and an I/O mode as its arguments, and it returns a
handle packed in the IOmonad:

~~~~ {.haskell}
Prelude> :type IO.openFile
IO.openFile
  :: FilePath
     -> GHC.IO.IOMode.IOMode
     -> IO GHC.IO.Handle.Types.Handle
~~~~

We use IO.ReadMode to open `brown.txt`{.filename} for reading:

~~~~ {.haskell}
Prelude> let h = IO.openFile "brown.txt" IO.ReadMode
Prelude> :type h
h :: IO GHC.IO.Handle.Types.Handle
~~~~

The handle is bound to h, but still in the IO monad. It would be nicer
if we can access that handle directly, but we told you that a value can
never escape its monad. Surprisingly, we can extract the value from the
IO monad in **ghci**. The reason that you can is that **ghci** lives in
the IO monad itself. So, the value will still never leave IO. We can
bind the value to a name (or pattern) using <-:

~~~~ {.haskell}
Prelude> h <- IO.openFile "brown.txt" IO.ReadMode
Prelude> :type h
h :: GHC.IO.Handle.Types.Handle
~~~~

That gives us the handle, bound to h. The next function that we will use
is IO.hGetContents, which returns unread data as a String wrapped in the
IO monad:

~~~~ {.haskell}
Prelude> :type IO.hGetContents
IO.hGetContents :: GHC.IO.Handle.Types.Handle -> IO String
~~~~

As we mentioned earlier, Haskell is a lazy language: expressions are
only evaluated when necessary. The same thing applies to I/O: the
relevant contents of a file are only read once you start extracting
characters from the String. With some smart programming, it is not
necessary for your program to read the whole file into memory, it will
allocate and deallocate chunks of the file as they are used. Now, get
the contents of the file:

~~~~ {.haskell}
Prelude> c <- IO.hGetContents h
Prelude> :type c
c :: String
~~~~

We can apply the usual list functions to this String:

~~~~ {.haskell}
Prelude> head c
'T'
Prelude> length c
6157180
~~~~

Since the file is sentence-splitted using newlines and tokenized using
spaces, you can use the `lines`{.function} and `words`{.function}
functions to apply sentence splitting and tokenization. For instance,
the first word of the corpus is:

~~~~ {.haskell}
Prelude> head (words (head (lines c)))
"The"
~~~~

The frequency of the word the is nicely wrapped in a Just constructor:

~~~~ {.haskell}
Prelude> Data.Map.lookup "the" (freqList (words c))
Just 62713
~~~~

Congratulations! This was your first venture into the world of corpus
statistics!

Todo: Zipfian distribution.
