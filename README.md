cl-acronyms
===========

An acronym expander that utilizes a templating system.

## How-to

After loading the system,
simply use `(acronyms:expand your-acronym-here)`
to generate an phrase
that has the first letter of each "main word" be `your-acronym-here`.
Optionally, providing a numerical argument
like `(acronyms:expand your-acronym-here iteration)`
will expand that acronym `iteration` times,
and collect all results into a list.

You can reload the list of words by evaluating `(refresh-list)`.
This will load a file named "mobiposi.i"
in the same directory as the code into the program.
You can find out how many entries the dictionary has
through `(total-entries)` and `(total-structures)`.
If for any reason you wish to delete the list without reloading a new one,
use `(reset-list)`.

## Dependencies

### mobyposi.i

cl-acronyms uses [mobyposi.i](http://icon.shef.ac.uk/Moby/mpos.html),
a list of words with tags that indicates each word's part of speech.
The version that this code uses the multiplication sign "�"
as the delimeter between words and tags, uses Unix file endings,
and was converted to use Unicode.
There are some known problems with the data,
particularly regarding letters with diacritics,
that proves difficult to fix given the sheer size of the file.

When loading the list,
`(refresh-list)` will automatically skip any entries that have spaces in them,
as those are unsuitable targets for an expansion of the acronym.

The file contains some unusual, uncommon or obsolete words
that may confuse a reader that might use it just for laughs.
Interested parties can provide a less confusing list
with a similar size.

cl-acronyms does not otherwise depend on any other library.

## Future

cl-acronyms remains a work in progress, though it works out of the box as it is.
Below are some of the things that are desired for future versions,
in no particular order:

* A more intelligent word structure system
* A list of sentence structures in the ballpark in scope as mobyposi.i
  * A slightly smaller mobyposi.i that doesn't use quite so many archaic words
* Greater control over how phrases are constructed:
  * Direct control over capitalization
* Direct control over spacing
