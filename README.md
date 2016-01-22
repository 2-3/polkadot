#polkadot
A lightweight personal wiki written in Racket. It stands at less than 200 LOC and its only dependency is Rackets "markdown"-package.


##Concept
polkadot displays __documents__ you have written in Markdown and links them to the __tags__ you specified in the document header.
There are no change logs or user accounts or similarly complex features---polkadot is meant as a simple personal wiki.
As such it doesn't aim to replace your whole publishing tool chain, but to be a part of it.

Following this philosophy, documents are simply files.
This makes it very easy to handle them in any way you want--edit them with your favorite editor, over SSH, FTP, or git.
Since documents are cached, there's no huge I/O overhead. polkadot also automagically updates its index on filesystem changes.

##Setup
Since it's a simple piece of software, polkadot is easy to install;

1. First you need to install the package "markdown" with raco: `raco pkg install markdown`
2. Now you should take a look at `polkadot.rkt` and change the configuration variables to your liking.
3. There's no further setup required, just write your documents and run the damn thing.

##Usage
At start polkadot will index all files in `documents/`.
It will continue to monitor this directory for any updated, added, or deleted files that end with `.polka`.
Static files can be added to `static/`.

You can find an introduction to writing documents in `introduction.polka`.
