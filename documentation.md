# Temporary

This will eventually become a proper PDF and less of a rambling mess. The same *caveat emptor* applies here as to the program, but even more. This is possibly outdated, incorrect, unrealised, unorganised or just un-.

# Timeline

For a full documentation, wait until February or March.
Pro úplnou dokumentaci v češtině počkejte do února až března. Český překlad se asi objeví spíš později.

# Evolution (and explanation) of the mechanics

The original idea comes from Paul Graham's spam filter (www.paulgraham.com/spam.html). It's a very basic text classification problem, simplified by there being just two categories and by the many distinctive features of spam. Graham came up with an idea that was revolutionary in 2002, that is, classification by Bayesian combination of probabilities of each word being spam.

The second inspiration is my absolute inability to find information about a given topic or from a given field. The internet is full of dictionaries, middle-school-level guides, thinly veiled advertising, and other trash, and all of this trash comes up often in search results. So, I thought, it should be easy to generalise the spam filter to filter text by topic.

I originally wanted to class using an existing hierarchical system, such as the Dewey decimal system, but no such system is designed for computer use. Dewey suffers from forcing exactly ten subcategories in every category, and, frankly, no such system accomodates the internet well. We need general-purpose blogs, amateur philosophy, and a bunch of other qualifiers that don't make sense in a library context.

So, there are two main tasks at hand. One, sensibly generalise Graham's idea. Two, design a useful hierarchy for articles on the Internet.

## Bayesian classification

We can decide between two classes using a Naive Bayes algorithm. Each class has an associated corpus, showing how many documents each word was featured in. We scale the larger corpus down to the size of the smaller corpus by multiplying all occurrences by the ratio of total documents in both classes.

For each word, we get the probability of it belonging to each category. To do so, we use Laplace smoothing. If a word if featured A times in the first category and B times in the second one, its score for the first category is (A + 1)/(A + B + 2). This has the benefit of probabilities summing to one, being close to just the ratio for well-known words and dealing gracefully with words that only occur in one corpus.

Combining the evidence from all words amplifies random noise. Combining the X most distinctive means that a the primary important thing is the amount of evidence, not its strength. So I settled on always picking the X words with the best and the X words with the worts score. This ensures evidence is balanced. To calculate X, find the number of words with score less than 0.2 or more than 0.8, then halve. This ensures a result like picking all words over an importance threshold, but with a significant bias towards uncertainty. It also means that when there is a lot of evidence, it is all considered (as is the case with legal boilerplate, where the case is so clear that the words don't fit in the explainer - there should be a way to browse it by pages). At least 6 words for/against are always taken, so the program will always at least try.

We multiply the score of all the chosen words, then scale the results up to sum to one. This is displayed. (a proper Bayesian illustration is pending)

## Tournament

Once we get the resulting scores for all pairs of classes, we combine them into a score for each class. The whole point of doing it this way, as opposed to the traditional classing between multiple folders (as in most tutorials, by the obvious generalisation of the pair classing system), is to throw out useless classes. For example, there is a three-way decision between legal boilerplate and two other categories. The site is decidedly not boilerplate, with nearly 100% certainty. We then want the program to behave as if the boilerplate category hadn't even existed. If we're doing generalised Bayes, this is a problem, because each word is compared to the total corpus, disregarding its distribution. When picking the 6 best and worst words, this led to both the folders getting boilerplate-like words as evidence against them. But this evidence should not be admitted if it makes a dismissed case - that the site is boilerplate - and if it doesn't differentiate between the two categories.

A PageRank-like algorithm is used. Each class initially gets equal probability. Then, in each iteration, each class gets a weighted average of its scores with all other classes, weighted by the scores of the respective opponent classes. The probabilities are scaled down to one. This converges reasonably quickly and predictably (starting with random scores led to the same results 200 times), though I'm not yet sure it always would. 

## Hierarchy

Classes are arranged as nested folders. For example, the boilerplate class contains classes "privacy-policy", "terms-of-use", and "cookies". Decisions are made within a folder between its subfolders, whose internal structure doesn't matter.

# Language choice

## Natural language

I study articles in English because there are many of them. English is a terrible language for this purpose, because a word can often have different meanings depending on context, but the problem exists in English, so English it is.

## Programming language

I use Common Lisp, a functional language. There are three paradigms I understand the point of: object-oriented, which can define complex data structures, but has trouble doing complex things with them (methods are not the primary focus); functional, which can nest operations in complex ways, but isn't good for complex data structures (the many functions need standardised input); imperative, which is what the computer ends up doing, has few benefits for the programmer, but is the most efficient and thus used in situations of limited resources.

This seems like a functional problem, for corpuses are just hash tables (dictionaries), sites are strings, and nothing much worse appears. CL is reasonably fast when optimised, at least compared to Python. There are also the necessary libraries available (not in the code yet) for HTML parsing.

# Crawler

The crawler inside is extremely poor. It is not configurable and presumably broken with the new backend.

# Current corpora

There is a *trash* folder, meaning content not intended to be found by this system. It consists of stuff found way too easily using regular search engines and of questionable quality (content mill SEO-reliant writing, dictionaries, unhelpful guides, etc.). For license reasons, only links to websites are provided, so you have to redownload data yourself with (redownload \*classes-folder\*).

# Used libraries

Dexador (https://github.com/fukamachi/dexador) for downloading page data.

LTK (http://www.peter-herth.de/ltk/index.html) for the window GUI.

QURI (https://github.com/fukamachi/quri) for dealing with URL character issues (example.com/B%C3%A1g%C5%99i -> example.com/Bágři)

Plump (https://github.com/Shinmera/plump) for something similar inside the HTML (XML escape: \&rsquo; -> ' (with a little help))

Trivial-timeout (https://common-lisp.net/project/trivial-timeout/) because the timeout from Dexador only works in a version that is incompatible with a working LTK.
