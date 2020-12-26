# Temporary

This will eventually become a proper PDF and less of a rambling mess. The same *caveat emptor* applies here as to the program, but even more. This is possibly outdated, incorrect, unrealised, unorganised or just un-.

# Timeline

This project will be in development until at least February, most likely March. Deadline for school is March 26th, with further fate unclear as of now. Ongoing modifications are available from the issues on Github.

# Purpose of this program

This is a Bayesian classifier meant to be primarily used for internet sites. It should be able to detect the class of a given site, typically reflecting its topic, style, or purpose. An eventual hope is also to enable the other direction, i.e., from a given class, run a crawler looking for such websites.

# Bayesian classification 

## Two classes

We can decide between two classes using a Naive Bayes algorithm. Each class contains example texts, which are scanned for tokens. Each class thus has an associated corpus with the number of occurrences of each word in its documents.

For naming purposes, one of the folder is the "target", while the other is the "control". The corpora are scaled to contain the same number of words overall (the larger folder has all its occurrence counts divided by the word count ratio).

When trying to decide whether a given text belongs to the target class, we also split it into tokens and look for the "score" of each token. The score represents the probability that a document with the word occurs in the target class. If the word occurs T times in the target class and C times in the control class, the score is (T + k)/(T + C + 2k), where k is a smoothing parameter ("Lidstone smoothing") meant to prevent probabilities of zero (by trial and error, k < 1 appears to be the best choice; I use k = 0.4).

Combining evidence from all words can lead to amplifying noise. Instead, only words over a threshold of interestingness (set as a score over 0.8 or under 0.2) are counted. An equal number of words over 0.5 and under 0.5 are considered, with the total number of words being the number of interesting words in total or a set value, whichever is greater. The set value is typically 10.

The scores for the target folder and for the opponent folder are found by multiplying all the interesting words' scores together and scaling the results such that they sum to one.

### Minutiae of corpus building

Each document can only contain a word once. There is some room for improvement here. Can we consider the absence of a word a feature? 

## Multiple classes

Each class is considered against every other class, and the resulting probabilities are multiplied together. This is based on the assumption that the correct class should beat every other class, so all classes are equally valid as controls. It also means that adding a new class that is most definitely not the correct one (all probabilities against it will be close to 1) doesn't affect the scores of other classes much. In practice, the scores between incorrect classes are hard to predict, but the winning class is clear enough.

## Hierarchical classing

If a class contains subclasses, their content is considered content of the parent class. Classification always works between the subfolders of a single folder, with their further internal structure being irrelevant.

The purpose of hierarchical classing is to class based on different criteria. For example, once we determine that a site comes from a content mill, and is thus probably low-quality, we don't need to class it by topic - specific topics in content mills are not a useful search criterion. 

# Data sourcing

There is a potential for bias in the source data. The program only understands a class in terms of the files that are placed within it, and thus can pick up on irrelevant clues. This problem cannot be fixed completely: If there existed an unbiased, comprehensive directory of the internet by topic to source the data from, there would be no need for this program to exist.

To mitigate it, several diversifying measures are used.

1) Source data from multiple domains.
2) Use crawlers to uncover mistakes, not amplify them. When a crawler is set to find articles in a given class, it tends to produce a mixture of correct and wildly incorrect results. Instead of inputting the correct results as base data, which would amplify any incidentals that helped with the correct classification, class the incorrect results in appropriate folders. In some cases, a persistent unclassifiable problem may force a new class to be created, as happened with news.
3) Try to use demonstrative websites. In some cases, a website's topic is just not reflected in its vocabulary. This can be caused by many interleaved topics, a lot of irrelevant content on the site (such as advertising for other content), or a heavy use of metaphor. Such websites should not be used for training, as their idiosyncracies won't be repeated elsewhere.
4) Try to divide any folder into different ways of approaching it. For instance, "food" can contain recipes, food safety, or nutrition. The lowest levels of the hierarchy are less important as targets (indeed, with under 5 files, classing is myopic at best) and more as specifications of the variations on the topic that should be included.
5) Try to include these subclasses in equal amounts. An example of this principle failing is currently the "math" folder, which is dominated by discussions of googology that are easy to find from the googology Wikia.

# Crawling

Crawling functionality is provided. It respects robots.txt and properly identifies itself in request headers. It is somewhat accessible from the GUI, with some caveats (such as the GUI only responding once crawling is finished) and with redundancy in the code (to be fixed for the first release).

## Drunkbot

Simple bot that always follows a random link. Avoids visiting a given link twice and stays away fom social media (where it tended to get stuck). Demonstrates that some sites are rather difficult to leave.

## Wanderbot

Starts with a seed and is focused towards a category. Avoids ever visiting the same domain (somewhat clumsily defined as the penultimate part of the url, i.e. www.example.com -> example, except in the case of a trailing dot, such as "www.example.com."). Keeps a queue ranked by score (calculated by adding the probabilities of the URL belonging to the target class as it gets more specific).

The theory behind wanderbot is that to get a chance of finding the target, the bot must visit distinct parts of the internet, and not get stuck on a single domain. Once it manages to hit the mark at least sometimes, additional techniques might be deployed to start a more local search. 

# Design choices

## Language choice

### Natural language

I study articles in English because they are the most available. English is a terrible language for this purpose, because a word can often have different meanings depending on context, but the problem exists in English, so English it is.

### Programming language

I use Common Lisp, a functional language. There are three paradigms I understand the point of: object-oriented, which can define complex data structures, but has trouble doing complex things with them (methods are not the primary focus); functional, which can nest operations in complex ways, but isn't good for complex data structures (the many functions need standardised input); imperative, which is what the computer ends up doing, has few benefits for the programmer, but is the most efficient and thus used in situations of limited resources.

This seems like a functional problem, for corpuses are just hash tables (dictionaries), sites are strings, and nothing much worse appears. CL is reasonably fast when optimised, at least compared to Python.

# Used libraries

Drakma (https://github.com/edicl/drakma) for downloading page data.

LTK (http://www.peter-herth.de/ltk/index.html) for the window GUI.

QURI (https://github.com/fukamachi/quri) for dealing with URL character issues (example.com/B%C3%A1g%C5%99i -> example.com/Bágři)

Plump (https://github.com/Shinmera/plump) for something similar inside the HTML (XML escape: \&rsquo; -> ' (with a little help))

Trivial-timeout (https://common-lisp.net/project/trivial-timeout/) because the timeout from Dexador only works in a version that is incompatible with a working LTK.

# Other used software

Portacle
Github
