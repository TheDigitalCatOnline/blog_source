Title: Introduction to hashing
Date: 2017-10-18 19:00:00 +0100
Category: Programming
Tags: algorithms, Python, Python3
Authors: Leonardo Giordani
Slug: introduction-to-hashing
Summary: 

Have you ever used dictionaries or maps in your language of choice, or you met a mysterious MD5 code while downloading a file from a server. Maybe you are a programmer, and using Git to manage your code you ended up using strange numbers called SHA1, and surely you heard at least a couple of time the term _cache_, which probably needed to be emptied in your browser.

What do all these concept have in common?

In this post I want to introduce you to the concept of _hashing_, which is one of the basic topics a good programmer shall know. Hashes are such an important topic in computer science that lacking knowledge in this field means being confused about a wide range of other subjects, like cryptography and security. Just to name some of the players you could be interested in your future career as a programmer: Bitcoin and blockchains, HTTPS and SSL, public/private key. All these topics have hashing as one of their building blocks, so as you can see it is worth mastering the concept.

This will obviously be only a humble introduction to the subject matter, as the whole concept is too broad for a single post. You will find many resources at the end of the post for you to start a serious study of this important part of computer science.

The main problem that hashing helps to solve is that of efficiently store and retrieve information. With _information_ here I mean a string of bits, which can represent anything that can be represented digitally, like music, video, text, programs, scientific data, etc. I will give some examples taken from the physical world, but always keep in mind that the objects we are dealing with are bits, thus "just numbers".

When we talk about algorithms, the word _efficient_ can have multiple meanings and actually different solutions can be considered _good_ depending on the target we have. When dealing with data structures we usually consider three main parameters, which are storage size, insertion speed and retrieval speed.

# Rationale

The reason behind the invention of hashing techniques is the need of uniquely identifying content. The problem pops up in very different areas, but eventually it can be expressed as: how can we take an input string of bytes of any length and assign to it an unique identifier?

This is a problem that we have, for example, when we want to double check the integrity of something that we downloaded from Internet, like a Linux distribution. The ISO file is very big, and we cannot manually compare the content provided by the server (say for example a page with the hexadecimal dump of the file) with the content we downloaded. With an image of 4 GB, comparing 10 bytes per second, it would take almost 14 years of work, night and day without eating or sleeping. Not that intriguing, right?

Is it possible to "label" that string of bytes with somethign that is both **unique** and **short**?

# The naked truth

Let's enjoy some mathematical theory before diving into the practical solutions that we are currently using.

## Strings of bytes

First of all, I introduced the problem speaking about a string of bytes, or in other words, a sequence of numbers. You know that everything that can be represented in a computer is a sequence of binary digits, which is the meaning of the term _digital_. When we can transform something from the real world into a sequence of binary digits we say that we _digitalised_ it, and we can consider it as a number.

This means that we already _mapped_ real objects to numbers. What does mapping mean? Let's consider a sentence like

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

We may also convert it to binary numbers, which are what the computer really deal with

``` txt
1010100 1101000 1100101 100000 1001101 1100001 1100111 1101001 1100011 100000 1010111 1101111 1110010 1100100 1110011 100000 1100001 1110010 1100101 100000 1010011 1110001 1110101 1100101 1100001 1101101 1101001 1110011 1101000 100000 1001111 1110011 1110011 1101001 1100110 1110010 1100001 1100111 1100101
```

Mind that this is not _THE_ representation of the string in binary, but _A_ representation of the string in binary, or *the representation of the string in binary according to the ASCII encoding*. I want to stress here that I could come up with a different code and get a completely different binary number starting with the same input.

This is basically what we do with every type of content from the real world. We set up a standard, which is called _encoding_ that maps measurable values to integers and, ultimately, to binary numbers. The _decoding_ process, in turn, converts back the binary number into something that can be experienced in the real world.

When we capture a picture and divide it in a matrix of N pixels, and assign to each pixel a value of R(ed), G(reen) and B(lue) we are encoding the picture in RBG. There are other encodings for colours, like for example CMYK, that give completely different binary numbers for the same input pixel.

When dealing with computer data, then, we are accessing a _universe_ of possible values, which is made by all possible binary strings. In this universe **U** we can find every binary string of any TODO size. What is the size of this universe of data? Well, given that it contains _all_ strings of _any_ TODO size its size is infinite.

IS THIS TRUE? VVV
Remember that this is not the actual size of the set, but its _possible_ size. In other words, I'm not planning to actually store all the possible strings, but I have to be aware of the fact that the strings I will store may come from this infinite set. This will be very important later.

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

## Sometitle

Let's go back to our set of all possible strings. As we said this set is infinite, because there is no limit to the size of the strings contained. If we want to organise this set in the smart way we described in the previous section we have to come up with a unique numeric index for each contained element.

The strict requirement, here, is that the index has to be unique, which means that two different elements of the set cannot have the same index. We can achieve this simply ordering alphabetically all possible lists and assignign them a progressive number: 1, 2, 3 ...

Unfortunately, since the possibile size of the set is infinite, the number of indexes we will use will be infinite as well. We cannot represent infinitely big numbers in a computer, so we have a problem.

STUCK STUCK STUCK STUCK STUCK STUCK STUCK STUCK STUCK STUCK STUCK STUCK STUCK STUCK STUCK STUCK STUCK STUCK STUCK STUCK STUCK STUCK STUCK STUCK STUCK STUCK STUCK STUCK STUCK STUCK STUCK how can I explain that the size of the universe is infinite so we need to compress it to a smaller one?




Then we fill a document with all the books in alphabetical order (by title and author) and the relative index.

When we need to find a book we just open the document, find the book we need in the list (which is easy because of the alphabetical order) and go directly to the correct room and shelf. 


We now want to index the content of this universe, which means that we want to assign to each element a unique number, so that we can refer to the number instead of the element. The idea is that if instead of mentioning a very long binary string like a whole book or the digitalised content of a picture we could mention an index, we would achieve a tremendous improvement. When content is _indexed_ we can go straight to its position in the storage instead of reading every single element to find the one we are looking for.

DRAWING LINKED LIST VS ARRAY

Let's then say that we devise some mathematical way to assign a _unique numeric index_ to each element of the universe **U**. We can order all the elements according to this index and store them.

When we store some content we just need to compute the index and keep it. When later we want to retrieve that same content we can directly go to the slot at that index and retrieve the value.

DRAWING STORING AND RETRIEVING CONTENT

