# Is everything a number?


The formal definition of hash function is: any function that can be used to map data of arbitrary size to data of fixed size. This definition encompasses a wide range of mathematical functions or algorithms, but all of them process numbers and return numbers. In the previous example, however, we used them to process a string and an ISO file. How is this possible?

You know that everything that can be represented in a computer is a sequence of binary digits, which is the meaning of the term _digital_. When we can transform something from the real world into a sequence of binary digits we say that we _digitalised_ it, and we can consider it as a number.

In the digital world we _map_ real objects to numbers. What does mapping mean? Let's consider a sentence like

``` txt
The Magic Words are Squeamish Ossifrage
```

Back in 1963 someone in the US came up with a code called ASCII which was used to convert characters of the English alphabet into numbers. ASCII is a (very small) subset of Unicode UTF-8, which is the standard that we use nowadays, so it is still valid, and I will shamelessly use it for this example.

Mapping the previous string with ASCII means to assign to each letter the numeric code given by the standard. In this case the result is

``` txt
84 104 101 32 77 97 103 105 99 32 87 111 114 100 115 32 97 114 101 32 83 113 117
101 97 109 105 115 104 32 79 115 115 105 102 114 97 103 101
```

Please note that it is custom to use hexadecimal notation, which allows to represent each symbol in the ASCII (one byte) with exactly two digits. So the string becomes

``` txt
54 68 65 20 4d 61 67 69 63 20 57 6f 72 64 73 20 61 72 65 20
53 71 75 65 61 6d 69 73 68 20 4f 73 73 69 66 72 61 67 65
```

We may also convert it to binary numbers, which are what the computer really deals with

``` txt
1010100 1101000 1100101 0100000 1001101 1100001 1100111 1101001
1100011 0100000 1010111 1101111 1110010 1100100 1110011 0100000
1100001 1110010 1100101 0100000 1010011 1110001 1110101 1100101
1100001 1101101 1101001 1110011 1101000 0100000 1001111 1110011
1110011 1101001 1100110 1110010 1100001 1100111 1100101
```

Mind that this is not _THE_ representation of the string in binary, but _A_ representation of the string in binary, or *the representation of the string in binary according to the ASCII encoding*. I want to stress here that I could come up with a different code and get a completely different binary number starting with the same input.

This is basically what we do with every type of content from the real world. We set up a table, which is called _encoding_ that maps measurable values to integers and, ultimately, to binary numbers. The _decoding_ process, in turn, converts the binary number back into something that can be experienced in the real world.

When we capture a picture and divide it in a matrix of N pixels, and assign to each pixel a value of R(ed), G(reen) and B(lue) we are encoding the picture in RBG. There are other encodings for colours, like for example CMYK, that give completely different binary numbers for the same input pixel.

Everything in a computer is ultimately a number, even though different conversion standard can return different numbers. This means that we can apply mathematical concepts to computer data.
