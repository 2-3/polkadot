#polkadot
A lightweight personal wiki written in Racket. It stands at around 150 LOC and its only dependency is Rackets "markdown"-package.


##Concept
polkadot displays __documents__ you have written in Markdown and links them to the __tags__ you specified in the document header.
There are no change logs or user accounts or similarly complex features---polkadot is meant as a simple personal wiki.
 
###Future features
While polkadot will at its heart always remain just a simple personal wiki,
there are a few features that are planned to be included in future versions:

- Wiki-style links to documents and tags.
- A page listing the most recently changed documents, to add a more traditional blog-like interface. 
- Static page generation

##Setup
Since it's a simple piece of software, polkadot is easy to install;

1. First you need to install the package "markdown" with raco: `raco pkg install markdown`
2. Now you should take a look at `polkadot.rkt` and change the configuration variables to your liking.
3. There's no further setup required, just write your documents and run the damn thing.

##Usage
At start polkadot will index all files in `documents/`.
It will continue to monitor this directory for any updated, added, or deleted files that end with `.polka`.

Writing documents is simple, take a look at `readme.polka` for an example.
Documents are markdown-formatted with the exception of the header, which consists of plain old S-expressions.
These get evaluated in a fresh namespace and should return a hash named `polka-metadata` which contains the documents metadata.