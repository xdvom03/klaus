# Temporary

This will eventually become a proper PDF, less of a rambling mess, and also probably more serious in language. The same *caveat emptor* applies here as to the program, but even more. This is possibly outdated, incorrect, unrealised, unorganised or just un-.

# Timeline

For a full documentation, wait until February or March.
Pro úplnou dokumentaci v češtině počkejte do února až března. Český překlad se asi objeví spíš později.

# Evolution (and explanation) of the mechanics

The original idea comes from Paul Graham's spam filter (www.paulgraham.com/spam.html). It's a very basic text classification problem, simplified by there being just two categories and by the many distinctive features of spam. Graham came up with an idea that was revolutionary in 2002, that is, classification by Bayesian combination of probabilities of each word being spam.

The second inspiration is my absolute inability to find information about a given topic or from a given field. The internet is full of dictionaries, middle-school-level guides, thinly veiled advertising, and other trash, and all of this trash comes up often in search results. So, I thought, it should be easy to generalise the spam filter to filter text by topic.

I originally wanted to class using an existing hierarchical system, such as the Dewey decimal system, but no such system is designed for computer use. Dewey suffers from forcing exactly ten subcategories in every category, and, frankly, no such system accomodates the internet well. We need general-purpose blogs, amateur philosophy, and a bunch of other qualifiers that don't make sense in a library context.

So, there are two main tasks at hand. One, sensibly generalise Graham's idea. Two, design a useful hierarchy for articles on the Internet.

Graham's design has crippling flaws, such as giving a fixed probability to words in only one corpus, not making it dependent on how many times it is there. The system I want to use is pitting categories against each other in pairs, getting probabilities in every combination, and then combining them.

Getting the probabilities first requires normalising the corpuses. Each folder has an associated corpus generated from articles classed by hand. We scale the larger corpus down to the size of the smaller one. Then, we add one to all counts of words (Laplace smoothing, most affecting the words we don't know much). We divide the number of words in one corpus by the total number of instances of the word, getting the word's probability score for the corpus. We do this for all words and both corpuses. For both corpuses, we then pick the 6 words with highest probabilities and 6 with the lowest. This may need significant tuning. We multiply the probabilities together, then scale them up to sum to one (a proper explanation of why this is legit Bayes pending). This gives us scores for each pair.

Combining the evidence of the pairs is tough. A PageRank-like system will be implemented, where we want each folder to have the score determined by a weighted average of its paired scores, with the weights given by the scores of the respective other folders (clarifying illustration pending). This creattes a system of N equations, where N is the number of folders. These equations don't seem solvable, not even by hand for N=3 (the first nontrivial case). Just start with equal probabilities, then redistribute them by the weights, and hope it converges. It turns out not to, presumably needing some normalisation.

# Language choice

I study articles in English because there are many of them. English is a terrible language for this purpose, because a word can often have different meanings depending on context, but the problem exists in English, so English it is.

I use Common Lisp, a functional language. There are three paradigms I understand the point of: object-oriented, which can define complex data structures, but has trouble doing complex things with them (methods are not the primary focus); functional, which can nest operations in complex ways, but isn't good for complex data structures (the many functions need standardised input); imperative, which is what the computer ends up doing, has few benefits for the programmer, but is the most efficient and thus used in situations of limited resources.

This seems like a functional problem, for corpuses are just hash tables (dictionaries), sites are strings, and nothing much worse appears. CL is reasonably fast when optimised, at least compared to Python. There are also the necessary libraries available (not in the code yet) for HTML parsing.
