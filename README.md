# Klaus

# What is this?

A text classification system that allows you to define a hierarchy of classes, classify text within it, and search the Internet for sites belonging to a given class.

# Status

Klaus was a project for the *maturita* exam from IT at Gymnázium Jana Keplera. It was finished on April 7th, 2021, and defended on April 21st, 2021. It may see some further changes; I would particularly like to translate the documentation to English and to make the code friendlier (better tests & documentation). I also hope to find the project useful in practice, in which case it will get periodically updated as I build a web directory.

# Dependencies

SBCL, wish. Both available through APT. I recommend using Linux as the Windows install process is more convoluted and fragile.

# How to use?

Download files. Extract a release (preferrably the latest one going by the release name (DD-MM-YYYY)) and run the executable. For building from source, see the documentation.

# Caveat emptor
There is no guarantee of a working state. There are known missing features in the code. The graphical interface can be... idiosyncratic. The program *should not* crash except in documented cases (trying to load a non-text file). Don't panic. Refer to issues, but also beware of *unknown* bugs.

The attached class hierarchy is WIP: incomplete, at times strangely subclassed.
