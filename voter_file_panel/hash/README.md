# Script for hashing Twitter IDs

This folder contains a Python script for hashing a set of Twitter IDs. Given
a file, `filename.txt`, containing exactly one ID per line (no extraneous
whitespace, etc.), usage is as follows:

```
python hash.py filename.txt <salt> <output_filename>
```

(This script requires Python 3; if you get an error try using
`python3` explicitly instead.)

This script appends the salt to each ID string, hashes the salted string,
sorts the list of hashes, then writes those sorted hashes to a file. 

We are interested in the set intersection of hashes, so it's convenient
to sort this output before writing it to a file.

```
python hash.py a.txt $(cat salt.txt) hashed_a.txt
python hash.py b.txt $(cat salt.txt) hashed_b.txt
comm -12 hashed_a.txt hashed_b.txt | wc -l # the size of the intersection
```

### A note on securing the salt

The salt is passed to the script in plaintext. To keep the salt secure, do
one of two things:

1. prepend a space to the Rscript call. This prevents the plaintext salt
   from being stored in your `.bash_history`.
2. write the salt (without the quotation marks) to a file, give it secure
   permissions, and read it directly:

```
chmod 400 salt.txt 
python hash.R filename.txt $(cat salt.txt) output.txt
```

