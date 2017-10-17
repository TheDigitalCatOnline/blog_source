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







-------------------------------------


identify pieces of information in a **quick**, **deterministic** and **approximate** way. Let's give a quick real-world example, and then discuss each of these requirements.

