Title: From hashing to sharding
Date: 2017-10-06 18:00:00 +0100
Category: Programming, Projects
Tags: algorithms, Python, Python3
Authors: Leonardo Giordani
Slug: from-hashing-to-sharding
Summary: 

Have you ever used dictionaries or maps in your language of choice, or you met a mysterious MD5 code while downloading a file from a server. Maybe you are a programmer, and using Git to manage your code you ended up using strange numbers called SHA1, and surely you heard at least a couple of time the term _cache_, which probably needed to be emptied in your browser.

What do all these concept have in common?

In this post I want to introduce you to the concept of _hashing_, which is one of the basic topics a good programmer shall know. Hashes are such an important topic in computer science that lacking knowledge in this field means being confused about a wide range of other subjects, like cryptography and security. Just to name some of the players you could be interested in your future career as a programmer: Bitcoin and blockchains, HTTPS and SSL, public/private key. All these topics have hashing as one of their building blocks, so as you can see it is worth mastering the concept.

This will obviously be only a humble introduction to the subject matter, as the whole concept is too broad for a single post. You will find many resources at the end of the post for you to start a serious study of this important part of computer science.

The main problem that hashing helps to solve is that of efficiently store and retrieve information. With "information" here I mean a string of bits, which can represent anything that can be represented digitally, like music, video, text, programs, scientific data, etc. I will give some examples taken from the physical world, but always keep in mind that the objects we are dealing with are bits, thus "just numbers".

When we talk about algorithms, the word _efficient_ can have multiple meanings and actually different solutions can be considered _good_ depending on the target we have. When dealing with data structures we usually consider three main parameters, which are storage size, insertion speed and retrieval speed.

# Another dimension

We have been kidnapped by a strange race of aliens, who brought us in a parallel dimension where we are given the task of managing the most important library of the whole galaxy (if you think this is a strange plot you should read the Hitchhiker's Guide to the Galaxy by Douglas Adams). Well, it looks like these aliens are trying to setup this galactic library and do not know how to organise it, so we have to come up with a solution. The good news is that they are going to pay us for the job!

Let's start considering the problem. Our information is represented by books, and what we do is to _insert_ them in the library when we acquire them, _retrieve_ them when someone wants to read them, and _manage_ the library itself (i.e. the physical structure of the building). We have an initial meeting with our kidnappers to understand the amount of books they expect to manage and we are pleased to understand the number 128 (we already learned the language, but we do not master it yet).

With 128 books to manage the simplest solution for the library is that of a single shelf where books are put next to each other. While the collection grows we increase the length of the shelf asking the local carpenter; in this dimension all books have the same size (don't ask, I don't make the rules) so the total size of the library is `S*N`, where `S` is the size of one book and `N` is the number of books.

When we acquire a new book we just need to go to the current end of the shelf and put the book there. As in this parallel dimension we can teleport, the size of the shelf is not a problem, and we consider the insertion to be 1 single action. Because of the strange language we talk in our dimension this concept is written `O(1)` and it is pronounced "O 1" (o one), or, in the posh clubs, "big o of 1".

We are pretty satisfied with our library, then. Managing it requires the minimum possible space and putting books into it requires a single action every time (the minimum number of possible actions).

Then, one day, one of the inhabitants of our dimension comes and asks us to give them one of the books. This is a library, after all, and lending books is its purpose. We politely ask the title of the book and then face the problem or finding it. It turns out that the only solution is to look at each one of them in order and see if we can find the one that we are looking for. We are however lucky, and we find it in the 4th position. Wow, it wasn't that bad, was it? It required only 4 actions!

The day after another inhabitant comes and asks for another book. This time we are not lucky, as the book is almost at the end of the shelf! We had to look at all the books in the collections and it took several days to find the requested book. So we consider our library and we think that while it is very good when it comes to inserting books, it is a bad when retrieving them, as _in the worst case_ it takes `N` actions, where `N` is the number of books. In the peculiar language spoken in this dimension this concept is written `O(n)` and it is read "O n" (o en).

In the meanwhile we have a chat with our kidnappers (now our paying employers), and we understand that the amount of books is not `128` as we understood, but `2^128`, as in this dimension people use often powers of `2` to express numbers (weird!).

Our solution is definitely too simple and not really effective. It turns out that `2^128` is a huge number, something around hundred of billions of billions of billions of billions. So much for preserving the culture! Ever heard of computers, dear aliens?

Thus we meet and we try to come up with some solution. Suddenly we think at dictionaries and the alphabet. We should have thought of it from day zero. We just have to divide the library in several sections each named after a letter of the alphabet. Then we will put all the books which title starts with that same letter in the relative section. The position of all the sections is written of a sheet on our desk, so we can find the right one very quickly (and move there instantaneously thanks to our dimension's peculiar rules). Inside each section there is a very long shelf with all the books, just like it happened before.

Let's check what happens to our daily routine. First of all, when we acquire a new book we have to match the first letter of the title with the relative section's position, looking at the table on our sheet (1 action). Then we move to the section and put the book at the and of the shelf, which costs us 1 single action. So the cost of one insertion increased from 1 to 2. The good news is that it is _always_ 2, independently of the size of the library, and we call this **constant** time. Because languages are weird, this is called `O(1)` even when the amount of actions is 2. In other words, `O(1)` means constant.

What happens when it comes to retrieve books? A customer asks for a specific title, so what we need to do is similar to the previous case. We first match the first letter of the title with the correct section (1 action), then we move to the relative section. And here we have the same problem we had before: we need to check all the books one by one! Yes, the section contains less books than the whole library, but while the section grows the problem remains the same, which is that our performances decrease directly with the number of books (`O(n)`). Given the number of books, then, dividing the library in 20-30 sections is not going to help.






We thus go back to the pool of scientists who are now spending their time on a beach, proud of their new algorithm, and yell at them until they go back to work. We need a real improvement in out library management, not a phony algorithm that doesn't change anything (but costs millions in refurbishing).

The scientists are good, however, and when pushed enough they can deliver effective solutions. So this time they come back with a small machine, a sort of keyboard with a small display. "This machine", says the TODO, "can convert the title of a book into a code that is almost always unique in the universe." Well, if this is TODO not a real breakthrough!

Our plan is then to fill the library of small wall-mounted slots TODO, each one labelled with a progressive number. We will then insert each book in the machine, get the unique slot number, and put the book in that slot. In the very rare case two titles give the same slot number (which should happen for a ridiculously small number of books) we will simply put both in the same slot. Insertion, thus, will cost just 2 actions (creating the code and storing the book), which we already identified as `O(1)`.

When someone comes asking for a book we will type the title on the keyboard of the machine the scientists provided, get the code and grab the book from the correct slot. Surprisingly, this also takes 2 actions! If two books occupy the same slot we obviously need to look at each one of them to decide which is then one we need, and in that case the number of actions increases with the number of books. We have been assured, however, that it is really rare for two different titles to be converted to the same slot number.

# Hashing

Back to the real world




--------------------------------





You are managing a collection of a couple of thousands text files covering several topics, in different languages. If you are given a new text file, how can you know if you already have it in your collection? And if you are given a new collection of similar size, how can you survive the dreadful task of merging the two?

Well, you _could_ in principle just open the first book, try to understand what the content is (good luck if you don't know the language), then with the help of some previously created tags identify the subset of books that cover the same topics and open them one by one, trying to figure if the new entry is a duplicate. If you do not have the tags (because you are the first one that accesses the collection) the only method you can employ is that of checking _every single book_. This means that, in our example, you have to open thousands of text files for just one new entry. You can figure the amount of work you are facing when it comes to merging two equally sized collections.

But, I hear you screaming, I have a computer, so I can automatically compare files. This is true and actually this makes the whole process a bit easier for you, but algorithmically the problem remains unsolved, as we are still comparing every new entry with the whole collection.

@@@ Mathematically such an algorithm is said to be O(n), as its computing time depends directly on the amount of elements in the collection (n). So, if I double the collection **on average** I will double the processing time.

A better solution could be the following. We first group text files according to some property of their binary content, for example the two first bits. Then, for each new entry we look at the first two bits, we find the group, and then compare the new file with all the files in the group. With two bits we can create four combinations (`00`, `01`, `11`, `10`) so the initial processing will create 4 groups (note that we will do this only once) and on average we have to compare our text file with one fourth of the whole collection.

@@@ I am purposely skipping a lot of mathematical concepts and brutally approximating results, as I want you to grasp the concept more than the gory details.

If we do it on the first 8 bits we can create `2^8 = 256` groups, and we will need to compare the file with only 1/256 of the collection. The idea is pretty old, and if you ever opened a dictionary (a real one, printed on paper) you have a perfect example of this algorithm. The dictionary is divided in sections according to the first letter of the word, so that you have to search through a fraction of the whole set of words of your language. Leave aside, for the moment, the fact that words are ordered alphabetically inside the sections, we will come back to this concept later.

So we could decide to check a given number of bits (say for example 4) at the beginning of the file and divide the collection according to the result. If the number is large enough the number of groups will become huge and, and this could finally make the collection searchable.

Well, no. Actually this is true in a very trivial case, that of _well distributed_ content. This means that given N groups (`N = 2^4 = 16` in the example) roughly 1/N (`1/16`) of the files begin with the first combination of bits (`0000`), roughly 1/N of the files begin with the second combination (`0001`), and so on. This may be true, but it is very unlikely, or at least is an assumption that we cannot generally make. As a simple example, all my `bash` files start with `#!/bin/bash`, and the first 4 bits are then always `0010` (`00100011` or `0x23` is the `#` character, aptly called **hash**).

One solution to this problem is to increase the number of bits that we consider, thus also increasing the number of groups that we are creating, and actually this is a method that works if we know in advance the content of the files, and can then devise how many bits will be sufficient. In the previous example, as all the files start with the same string of 12 characters I could decide to consider the first 256 characters and be happy (if I am sure that my files do not all start with the same comment of 256 characters!).

Truth is, the only way to be reasonably sure that we are not considering too small a number of bits is to use the whole file, regardless of its length. If we do this, the _key_ we use to search for the content is the content itself. This makes the content very well distributed, _perfectly_ distributed, as there are as many groups as the number of files, and indeed this type of hashing is called **perfect hashing**. This nullifies however the benefits of the algorithm, as we need now to compare the full content of the file (the search key) with the full content of all the files (the index keys of the collection).

Is there a way to solve this situation? To state the problem in a better way, is there an indexing function that accepts content of any length and that given as a result a good distribution of the content across groups?

# Hash functions

The formal definition of hash function is mostly stated by the problem itself. A hash function is any set of operations that converts data of arbitrary length into data of fixed length. This behaviour solves the first requirement by definition, as there is no limit to the length of the input data, but leaves the second requirement as a quality parameter of the hash function itself.

For example, the function that we used in the first example is a hash function, as it takes an input of any length and converts it into a 2 bit string (fixed length), and it is a good hash function under the assumption that the first two bits of the content are distributed evenly. This assumption can't be easily taken for granted, and this is the fault of all the hash functions that considers a limited number of bits (whichever position they are taken from in the file).

A better example of hash function are MD5, which computes a 128-bit value starting from data of any size, and SHA-1, which computes a 160-bit one.

To see a practical example of use we can try to compute the MD5 hash of some files. In the Linux distributions the program `md5sum` is usually part of the core system (in Ubuntu it is included in the `coreutils` package). 

Create three files, `short1.txt`, `short2.txt`, and `long1.txt`. You can fill them with random data with the following commands

```
$ dd if=/dev/urandom of=short1.txt count=1024 bs=1
$ dd if=/dev/urandom of=short2.txt count=1024 bs=1
$ dd if=/dev/urandom of=long1.txt count=1024 bs=1024
```

Now you have two "short" files of 1 kilobyte and a "long" one of 1 megabyte. If you now compute the MD5 checksum of each file you will obtain 3 equally sized strings

``` sh
$ md5sum short1.txt
b66000b03cf69047b0de2ed258177e61  short1.txt
$ md5sum short2.txt 
64fe56ddcfa52f5380b490259ee6e398  short2.txt
$ md5sum long1.txt 
9f48230c162719a97944172422e47512  long1.txt
```

Keep in mind your results will be different because of the random data we used to populate the files.

# Collisions

A hash function is said to have a collision when two different inputs produce the same hash. Every hash function (apart from the perfect ones) has collisions _because of their own nature_. A hash function reduces an input of any size to a sequence of values of a given size, so while there are infinite possible inputs there is only a finite set of outputs. Thus, some inputs correspond to the same output (i.e. the mathematical function is not injective).

Let's consider MD5. As I said, the algorithm produces a 128-bit string, which allow for `2^128` different values, 340 billions of billions of billions of billions of values. This is a considerable amount, but it is far from being infinite, so we shall expect two different inputs to produce the same output.

For some hash functions creating a collision is very easy. Consider for example the trivial function that I presented at the beginning of the post, which considers a given number of bits at the beginning of the input data: to create a collision you simply need to set those bits. For functions that use the whole input data, like MD5 or SHA-1, creating a collision requires a remarkably greater effort, many times too great to be feasible.

The difficulty involved in purposely creating two inputs that produce the same hash is a quality parameter of _cryptographic hash functions_, where hash functions are used to certify the integrity of messages and their source. If a method to create a collision is known, a malicious attacker could take advantage of it to produce a different version of a message that would be however not recognised as tampered. The aforementioned MD5 and SHA-1 functions, for example, have been both successfully attacked, that is someone devised a method to create a tampered message with the same hash of another one in a feasible way (which means with affordable resources in an affordable time).

# Hash functions and dictionaries

Hash functions are used in _hash tables_, which are for example used in the current implementation of Python dictionaries. The idea behind a hash table is the one that I described at the beginning of the post. Having a series of slots indexed with a key and then storing the content in the slot that corresponds to its hash value, i.e. the result of a given hash function applied to the input itself.

An implementation of hash tables has to take into consideration, mostly related to the speed of storage and retrieval, but these considerations are outside the scope of this post so I will defer them to a future analysis. For the moment let us consider the simpler dictionary implementation which is made by a hash function and a series of arrays (an array of arrays).

``` python

# EXAMPLE OF DICTIONARY WITH hash() and list()

```

This structure is very simple, and it can be greatly improved employing better structures for the slots, or introducing different considerations about the nature of input data. In principle, however, this is the way every hash table works.

# Adding and removing slots

One of the problems that arise when creating a hash table is how to resize it, as it is very impractical to allocate the full space for the slots when the table is empty. Let's consider MD5, for example: the hash values cover a space of `2^128` different values, and storing a single 32-bit integer for each slot would require `2^128 * 2^32 = 2^160` bytes. This number doesn't probably seem too scary, but consider that 4 gigabytes of memory correspond to `2^32` bytes and that `2^160` is that that quantity raised to the power of 5. Like, we need billions of billions of billions of billions of 4 gigabytes slots to host this structure. And we didn't yet start to put the content in the table!





identify pieces of information in a **quick**, **deterministic** and **approximate** way. Let's give a quick real-world example, and then discuss each of these requirements.





When dealing with computer data, then, we are accessing a _universe_ of possible values, which is made by all possible binary strings. In this universe **U** we can find every binary string of any TODO size. What is the size of this universe of data? Well, given that it contains _all_ strings of _any_ TODO size its size is infinite.

IS THIS TRUE? VVV
Remember that this is not the actual size of the set, but its _possible_ size. In other words, I'm not planning to actually store all the possible strings, but I have to be aware of the fact that the strings I will store may come from this infinite set. This will be very important later.


The main problem that hashing helps to solve is that of efficiently store and retrieve information. With _information_ here I mean a string of bits, which can represent anything that can be represented digitally, like music, video, text, programs, scientific data, etc. I will give some examples taken from the physical world, but always keep in mind that the objects we are dealing with are bits, thus "just numbers".

When we talk about algorithms, the word _efficient_ can have multiple meanings and actually different solutions can be considered _good_ depending on the target we have. When dealing with data structures we usually consider three main parameters, which are storage size, insertion speed and retrieval speed.


A hash has some very interesting properties. Let me briefly introduce them before we properly dive into the matter:

## Fixed length

The length of the hash number depends only on the algorithm (or on a parameter of a given algorithm) and not on the length of the input data. So both the MD5 hash of a 1 kbyte file and the one of a 4 gigabytes file are 32 characters long, while the SHA-512 hash of the two files are both 128 bytes long.

## Immutable

Given the algorithm, the hash number of some data is always the same. The result of the hashing function depends **only** on the data itself, and not on other parameters like time or computer system.

## Total TODO

Changing one single bit of the source data results in a complete change of the hash number. Compare for example the MD5 hash values of two similar strings

```
The quick brown fox jumps over the lazy dog => 37c4b87edffc5d198ff5a185cee7ee09
The quick brown fox jumps over the lazy cog => 15546a0bcace46fd5e12ec29adca5e70
```

## Irreversible

Given the hash of some input data it is practically impossible to recover the original data. Here _practically impossible_ means that it is mathematically too complex or even really impossible to perform the reverse operation.

## Not unique

Surprised? Well, the result of a hash function is not unique, which means that two different inputs may give the same output. Thanks to the irreversible property, however, it is practically impossible to know which inputs provide a given output.

# TODO

Let's enjoy some mathematical theory before diving into the practical solutions that we are currently using.



## Performances

Let's discuss now how we can store and retrieve the content of set.

If we store it in an unordered way (picture a room with objects everywhere), to retrieve it we need to look at each element. This can result in a lucky find (if the element we are looking for is one of the first we consider), or can end up being a very unlucky chance TODO if the element is one of the last ones.

When considering algorithms we always take into account the **worst case** as we cannot hope to be lucky every time. If the objects are unordered, thus, the worst case is that our element is the last one, so we need to look at all of them. We represent this concept with the the notation `O(N)`, where N represents the size of the whole set. We also say that the algorithm performs in a **linear** way.

Let's look at an example to clarify this very important concept.

We are given the task to find a book in a library. The library contains `2^12` books (4096 in decimal) but unfortunately the librarian isnt' a very organised person, and the books are not in alphabetical order by title, nor ordered by author. They are stored in random order. This means that unfortunately we need to get each one of them, open it, read the title and the author and compare them with the ones of the book we want to find. if the book is not the right one, we have to put it back and proceed to the next one.

If we work very quickly we might achieve to spend 10 seconds to take a book, look at the title and put it back. In the worst case we have to look at every book in the library, and with `2^12` books this means 40960 seconds, that is 11 hours 22 minutes and 40 seconds.

If we double the size of the library the time to find a book doubles as well (in the worst case), and this is why we call this type of algorithms linear.

Are there better ways to organise a library?

## Indexing

It turns out we could give each book a progressive index (a code) and store the books in rooms and shelves that have the codes printed on them. When someone wants a book and has the relative code we just need to go and fetch the book from the right shelf. No need to scan the entire collection or to open books and check for the content.

The biggest improvement, however, is that _the amount of time we need to retrieve a book doesn't change with the size of the library_.

Let me state again this very important concept. If we know in advance the exact position of a book through its code, it doesn't matter how big the library is. It will always take the same time to get a book.

This is not that true in the physical world. If the library was several kilometers long, it would obviously take more time to reach the last room than to get a book from the first one. We assume here that we are working with a type of storage called *random access memory* (RAM). Such a storage provides access to all its parts in the same amount of time, and this is why our assumption is valid.

We represent the concept of _constant time_ saying that such an algorithm is `O(1)`, which means that in the _worst case_ the performances of the algorithm do not increase with the size of the data set.

Let's consider a concrete example of indexing. Consider the following set of strings:

```
ReplaceWithAString1
ReplaceWithAString2
ReplaceWithAString3
ReplaceWithAString4
ReplaceWithAString5
ReplaceWithAString6
```

In a computer program you could replicate them every time you need them, using a variable amount of space according to the size of the string itself, or you can store them in variables and reference the latter. In particular you can store them in an array `strings`

```
strings[0] = "ReplaceWithAString1"
strings[1] = "ReplaceWithAString2"
strings[2] = "ReplaceWithAString3"
strings[3] = "ReplaceWithAString4"
strings[4] = "ReplaceWithAString5"
strings[5] = "ReplaceWithAString6"
```

At this point what you need is only a number form `0` to `6`, which can be represented with 3 bits.

What we obtained is actually a **compression** of the strings, that now need _always_ just 3 bits to be represented. Accessing `strings[0]` or `strings[5]` requires the same amount of time (this is a specific feature guaranteeed by the hardware we use) so we achieved our goal.

If we want to **hash** a string, that is find its index, we just need ot compare the string with each string in the array until they match. At that point we have the string's index, or its _hash_.

## Sometitle

The above example shows a potential issue. To find the hash of a string in that scheme we need ot access a sort of global repository with all the strings. We then need to match the string we have with the ones that are stored to find the index. We _cannot obtain the index from the data only_.











Let's go back to our set of all possible strings.

When we manage generic data we cannot just list the possible values as we did before and give each of them a predefined index. Now we consider _all_ possible strings, and we want to devise a method that gives an index to each of them. Since we do


Let's go back to our set of all possible strings. As we said this set is infinite, because there is no limit to the size of the strings contained. If we want to organise this set in the smart way we described in the previous section we have to come up with a unique numeric index for each contained element.

The strict requirement, here, is that the index has to be unique, which means that two different elements of the set cannot have the same index. We can achieve this simply ordering alphabetically all possible lists and assignign them a progressive number: 1, 2, 3 ...

Unfortunately, since the possibile size of the set is infinite, the number of indexes we will use will be infinite as well. We cannot represent infinitely big numbers in a computer, so we have a problem.

Furthermore, we don't want the indexes that can be represented to be too long. A system that indexes a very simple string of 100 characters (100 bytes) with an index that requires several gigabytes to be represented is definitely not a good one.












STUCK STUCK STUCK STUCK STUCK STUCK STUCK STUCK STUCK STUCK STUCK STUCK STUCK STUCK STUCK STUCK STUCK STUCK STUCK STUCK STUCK STUCK STUCK STUCK STUCK STUCK STUCK STUCK STUCK STUCK STUCK how can I explain that the size of the universe is infinite so we need to compress it to a smaller one?




Then we fill a document with all the books in alphabetical order (by title and author) and the relative index.

When we need to find a book we just open the document, find the book we need in the list (which is easy because of the alphabetical order) and go directly to the correct room and shelf. 


We now want to index the content of this universe, which means that we want to assign to each element a unique number, so that we can refer to the number instead of the element. The idea is that if instead of mentioning a very long binary string like a whole book or the digitalised content of a picture we could mention an index, we would achieve a tremendous improvement. When content is _indexed_ we can go straight to its position in the storage instead of reading every single element to find the one we are looking for.

DRAWING LINKED LIST VS ARRAY

Let's then say that we devise some mathematical way to assign a _unique numeric index_ to each element of the universe **U**. We can order all the elements according to this index and store them.

When we store some content we just need to compute the index and keep it. When later we want to retrieve that same content we can directly go to the slot at that index and retrieve the value.

DRAWING STORING AND RETRIEVING CONTENT



The reason behind the invention of hashing techniques is the need of uniquely identifying content. The problem pops up in very different areas, but eventually it can be expressed as: how can we take an input string of bytes of any length and assign to it an unique identifier?


# TITLE

First of all, I introduced the problem speaking of TODO a string of bytes, or in other words, a sequence of numbers. You know that everything that can be represented in a computer is a sequence of binary digits, which is the meaning of the term _digital_. When we can transform something from the real world into a sequence of binary digits we say that we _digitalised_ it, and we can consider it as a number.

In the digital world we _map_ real objects to numbers. What does mapping mean? Let's consider a sentence like

``` txt
The Magic Words are Squeamish Ossifrage
```

Back in 1963 someone in the US came up with a code called ASCII which was used to convert characters of the English alphabet into numbers. ASCII is a (very small) subset of Unicode UTF-8, which is the standard that we use nowadays, so it is still valid, and I will shamelessly use it for this example.

Mapping the previous string with ASCII means to assign to each letter the numeric code given by the standard. In this case the result is

``` txt
84 104 101 32 77 97 103 105 99 32 87 111 114 100 115 32 97 114 101 32 83 113 117 101 97 109 105 115 104 32 79 115 115 105 102 114 97 103 101
```

Please note that it is custom to use hexadecimal notation, which allows to represent each symbol in the ASCII (one byte) with exactly two digits. So the string becomes

``` txt
54 68 65 20 4d 61 67 69 63 20 57 6f 72 64 73 20 61 72 65 20 53 71 75 65 61 6d 69 73 68 20 4f 73 73 69 66 72 61 67 65
```

We may also convert it to binary numbers, which are what the computer really deals with

``` txt
1010100 1101000 1100101 100000 1001101 1100001 1100111 1101001 1100011 100000 1010111 1101111 1110010 1100100 1110011 100000 1100001 1110010 1100101 100000 1010011 1110001 1110101 1100101 1100001 1101101 1101001 1110011 1101000 100000 1001111 1110011 1110011 1101001 1100110 1110010 1100001 1100111 1100101
```

Mind that this is not _THE_ representation of the string in binary, but _A_ representation of the string in binary, or *the representation of the string in binary according to the ASCII encoding*. I want to stress here that I could come up with a different code and get a completely different binary number starting with the same input.

This is basically what we do with every type of content from the real world. We set up a table, which is called _encoding_ that maps measurable values to integers and, ultimately, to binary numbers. The _decoding_ process, in turn, converts back the binary number into something that can be experienced in the real world.

When we capture a picture and divide it in a matrix of N pixels, and assign to each pixel a value of R(ed), G(reen) and B(lue) we are encoding the picture in RBG. There are other encodings for colours, like for example CMYK, that give completely different binary numbers for the same input pixel.







Hash functions are also used in cryptography, but the requirements and the properties of cryptographic hash functions are obviously different from the ones of standard hash functions. In particular in cryptography given the hash of some input data it is practically impossible to recover the original data. Here _practically impossible_ means that it is mathematically too complex or even really impossible to perform the reverse operation. Such functions are usually called _one-way functions_ and are one of the most important parts of modern cryptography.


