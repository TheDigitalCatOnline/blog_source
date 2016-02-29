Title: Versioning - An underrated discipline
Date: 2013-03-20 10:02 +0100
Category: Programming
Tags: versioning
Authors: Leonardo Giordani
Slug: versioning-an-underrated-discipline
Version: 2
Summary:

## Overture

Everyone uses a computer has to deal in their everyday life with software version numbers, and even who deals with computers just as bare user shall sooner or later introduce the word _version_ in their speech. Alas, this made it one of those concepts so pervading that it is now taken for granted, and this is so widespread that even professionals often forget or do not even know the complexity of the matter behind. This article is an attempt to shine a light on this topic.

This post started as a collection of some thoughts that aimed to summarize the experience collected by me in 10 years of software development. As usual, short sentences proved to be too short and became long sentences, then paragraphs, and the short list was already a long article. However, despite the length my analysis does not claim to be either complete or exact.

It simply represents an attempt to introduce the reader to the complexity of a matter which is always relevant in the software world.

## A known concept?

Borrowing an example from an everyday context, speaking about versioning is like speaking about engine size of cars. Everyone roughly knows what engine size is and can give it both a relative value, comparing different sizes, and an absolute one. Many people, however, do not know the unit of measurement of engine size and almost no one among the common people (including me) knows the internal problems of an engine that arise when the size grows or decreases. While we can be satisfied by having a shallow knowledge of this topic, so that we can buy a car, identify it and drive it, the engineer who designs it or the mechanic who repairs it cannot stop at that level, but has to dive more into the subject.

Back to the software environment we can observe that everyone uses a computer even only for standard office work can roughly deal with different versions of the most common programs. Windows 7 is more recent that Windows XP; Photoshop CS2 is very old since the last version is CS6, and so on. As we will discover a rather complex world disguises behind those simple labels.

## A formal definition of versioning

Giving a **version number** (or, shortly, a **version**) intuitively means identifying the status of an object that may be modified in the course of time. In this sense, despite version numbers are widespread used in the software world, any object can be provided with one of them. A project of any nature, for example, or consumer goods can have an (explicit or implicit) version number.

A very nice example of this latter is that of game consoles, and it allows us to start unfolding some intricacies of the versioning matter. Sony Playstation is a game console which has seen four versions, simply named Playstation, Playstation 2, Playstation 3 and Playstation 4. When Microsoft entered the gaming industry with its Xbox console, Sony Playstation was on the market with its second version. About four years later, Sony was planning to release the third version of Playstation, and Microsoft answered with Xbox 360, which is probably going be followed by Xbox 720. The Xbox versioning system, even if based on an unusual sequence of numbers, does not prevent the consumer from understanding what is the most recent model of the console.

So now we can state more formally that **giving a version number means labelling the status of an object at a given moment in time according to a previously established set of rules**. This set of rules allows to find the correct version number and is called versioning system, versioning scheme or, more simply, versioning.

## The purpose of versioning

This latter formal definition leaves the question about the purpose of versioning open, i.e. the real benefit of applying that labelling. The reasons behind the necessity of univocally identifying the status of a given object are many and involve different aspects. Here, pure technical or scientific motivations mix with lesser formal decisions such as marketing ones.

Back to the game consoles example from above: Xbox version numbers have been conceived to transmit a marketing message, more than a technical one. The second version of Xbox, indeed, was designed to hinder the Playstation 3 sales. Thus, simply calling it Xbox 2 might have given the impression of an object older and less performing than the competitor. Xbox 360, then, may have multiple meanings from this point of view. The number 3 definitely refers to the version of the competitor’s console, but 360 also remembers the 360 degrees in a circle, suggesting that the console can do anything. As a consequence, the temporary name of the third version of Xbox is Xbox 720, continuing the sequence. The official name could be different, however, since it is not the first time Microsoft changes the versioning system of a product to match other products or different marketing strategies.

#### Adopting a versioning system

Let’s review some of the possible reasons for adopting a versioning system for your products.

##### Find out what has been used or distributed at a specific point in the past

This is the classic need of whom uses machines or software to produce other objects (either concrete objects or lesser material things like data, drawings, texts). It is always possible to find defects in the machine used to make the products as it is possible and desirable to upgrade it. At that point, it is very useful, when not mandatory, _to know what objects has been produced with the previous version_ of the machine or the software and when, where and to whom they have been distributed or sold.

Significant in this scope is the example of food, where entire lots are (physically) labelled in order to make it possible to go back to the origin, as an instance when sanitation problems arise. That way it is possible to withdraw the product from the market, reaching at the same time all customers who bought it. Similarly, when a software bug is discovered which opens a vulnerability in a system it is mandatory to find everyone uses the bugged version of the software to alert them and invite them to upgrade the installation.

##### Know the list of changes made in a period of time

In the software scope, from the programmer’s point of view it is always useful _to know what changes have been made between two versions_ to find out in an easy way what version introduced a bug or made a given functionality available. From the user’s point of view, it is useful to know the features introduced with a new version, either to know the new possibilities of the software or to evaluate the purchase.

##### Make easily recognisable different versions of the same model

When purchasing an object such as a car, the same model can be sold in different versions; each of them differs in some (more or less important) details: engine, fuel supply, color, etc. The producer, from its point of view, must be able _to tell each model apart_ to sell them or to service them.

## What does a version number represent?

With version number, we mean **a code which is usually made of digits, letters and symbols**, and which altogether identifies one and only one status of the labelled object. As in every code schema, however, it is necessary to state what the number or its parts represent. Especially important are the comparison rules between version numbers.

#### An example of versioning

To help our understanding of the matter let us make an example of the simplest versioning scheme, that is that made by a single integer number (a group of digits). The first version of the software (from now on we will look especially at this scope) will be labelled version `1`. The versioning system states then that the next version will be version `2`, then version `3` and so on.

Such a scheme seems to work, but gives little information. For example, it is not possible to know the extent of changes made between the two versions. Both after the fixing of minor bugs and a complete rewrite of the graphical interface of the software the version number increases by one. Neither it is possible to have information about the compatibility between different versions and taking for granted that no version breaks compatibility is a dangerous assumption.

So our simple versioning schema has some communication shortcomings. Such flaws, however, could be easily balanced by a good documentation that details the peculiarities of each version. This solution is definitely not elegant and immediate but would transmit all the necessary information.

#### Limits of the system

There is a serious structural flaw in our simple schema. Think about a software that has two versions, `1` and `2`, labelled according to the schema. For various reasons, many users are still using version `1` while version `2` has been available for a long time, for instance because the migration to the new version is hard or expensive.

In such a scenario, which could last for a long time, the developers find a serious bug in a piece of code which was not changed between the two versions. That bug is a dangerous issue for the software and affects both versions. The developers work on it and eventually fix it so now a new release of the software must be produced. For version `2` the bug fix results, according to the versioning system, in a new version called version `3`. But what about version `1`? How will the new version be called? It cannot be called version `2` since that label has already been assigned to another status of the code.

#### Evolving the system

As you can easily see, the problem has no solution if we keep using the given versioning system. Likewise, it is obvious that only when two or more versions of the same software are actively maintained such a problem arises; a software is actively maintained when new features are added to it, and its bugs are still fixed.

So a change is required. The first thing that comes to mind is to add to the version number another component that shows “minor” modifications. Pay attention to the fact that the “minor” adjective used here _refers to the software maintenance and distribution process  and not to an actual measure of the amount or quality of the new code_. Indeed there is usually some correlation between the amount of code and the extent of the change, but it is not so straightforward as it could seem at first sight.

A very broadly accepted and used method to add a new component to a version number is to add digits separating them from other components with a single dot.

Our simple versioning system thus enriches. The first version of the software will be `1.0`. A change that does not introduce an incompatibility will increase the rightmost number, producing versions `1.1`, `1.2`, and so on. A change that introduces an incompatibility will increase the leftmost number, producing versions `2.0`, `3.0`, and so on. This completely solves the previously described problem. After the bug fix the majority of our software users will run versions `1.1` and `2.1`, and only those users who did not yet upgrade the system will be running versions `1.0` and `2.0`.

## About numbering

A short digression about numbering. Generally we start counting from 1, and if you ask someone to tell you the first ten numbers, he or she will likely answer with the sequence 1,2,3,...,10. If you think carefully about it, however, the decimal system considers 0 the first number (0 units and 0 tens) and 10 the first of the sequence of numbers with 1 ten and units ranging from 0 to 9.

Obviously this system is valid as long as we use numbers as ordinals, that is _as labels_ (the first number is labelled `0`, the second is labelled `1`, and so on). From a cardinal point of view, i.e. speaking about the _size of a set_, starting from 1 is a must since the minimal concrete set is made by one object. I think it is good spending some time thinking about the subject since it is a very interesting matter in the programming scope too.

As an example, consider the difference between Fortran, COBOL and MATLAB (just to name some of the most famous ones) on one side, and C and derived languages on the other. The first set of languages index array elements starting from 1 while the second one index from 0 (while this is a well-known C feature, should be said that LISP first introduced it).

Unfortunately, there is an ambiguity in the common language since we name positions in a list starting from 1, thus mixing the cardinal and ordinal system. This is deep-rooted in the language: for instance in english the word "the third" comes from three, "the fourth" comes from four and so on ("the first” comes from "the foremost" and “the second” comes from the Latin secundus). If on the contrary we index elements starting from 0, now "the third" is the number two, the fourth is the number three, etc. This ambiguity comes from the rather late introduction of the number zero in western culture, officially around 1200. Just to show you that this point is less complex than it appears think about the hours you read on your digital clock; as you can see, they start from 0:00, not from 1:01. Indeed "the first hour" is hour number 0.

This ambiguity is very manifest in the versioning environment, where it is taken for granted that the first version of a software is numbered `1.0`. According to the common numbering system the first version should be numbered `1.1` while according to the formal system the first version should be numbered `0.0`. The problem that arises with version `0.0` is purely communicative since versions starting with 0 are usually considered not stable.

I think that the correct way of numbering versions is that starting from `0.0`, but I also consider `1.0` acceptable as the first stable version. This latter is a harmless decision and can spare many disagreements with marketing people.

## Incompatibility and versioning

Previously I talked about changes that introduce an incompatibility, but I did not formally defined this concept. **Incompatibility**, in the computer world, is the complete or partial impossibility of using a given version of a software (or hardware) in place of another. When evolving a software sometime it is necessary to change parts in a way that makes difficult or even impossible to replace the old version with the new one without changing some other element of the context or environment.

Let’s make a simple example taken from the everyday life of a common computer user. I write a document using Microsoft Word 2007 and save it in the native format, namely docx. Then I send the file to another person, who can only access Word 2003 and he or she cannot open it. Why? Simply because the changes introduced in Office 2007 to the file format are incompatible, that is they change the file format to such an extent that a previous version of the software cannot even show a part of the content.

As you can imagine, the fields where incompatibility problems can arise are many. Speaking in general _every part of a software (or hardware) that get in contact with the outside world can be affected by incompatibility issues_. That parts are altogether known as interfaces. According to the field of application you can have network interfaces, programming interfaces, hardware interfaces, etc.

An example borrowed from the hardware world is the following. If you design a new version of a hard drive, replacing the head with a totally new component that does not suffer from crashes, this is not incompatible with the previous version of the disk. Indeed hard disk heads do not get in contact with other external components, so the change is called transparent, that is _invisible to an external observer_. The new version of the hard drive can replace an old version without requiring changes to the hosting computer. On the contrary, if you replace the old IDE connector of the drive with a brand new SATA one you change a component that exchanges information with the outside world, namely the main and most important hard disk interface. That can represent a compatibility issue since to use the new hard disk model you probably have to replace the old IDE-only motherboard of your computer.

The versioning system of choice, thus, has to deal even with possible incompatibilities of our software’s interfaces, besides the maintenance aspects previously described.

Not every change made to interfaces produces an incompatibility. A **backward compatible interface** is able to act both as the new version and the old one (or ones), either automatically or manually driven. The aforementioned Office 2007 is backward compatible with Office 2003 since it can automatically read the old file format and can manually be driven to save a file in that format.

Pay attention that _users take for granted that your software is backward compatible with its previous versions_, so any incompatibility must be avoided if possible. When not, clearly state it in the documentation.

## Internal and external version numbers

As stated before, a version number is intimately bound to the way a software is maintained and distributed. A development team might however produce a substantial number of intermediate versions between two official releases, so it is important to tell apart external version numbers that bear a communication towards the user, from internal ones that transmit a communication towards the development team. Clearly each external version number will be coupled with one or more internal ones, but the opposite is not true.

So external version numbers address the user; thus they should above all permit to identify the extent of changes made to the software while using a schema as simple as possible. External version numbers can also be simple text labels as happened when the successor of Windows 2000 was named Windows XP. As for internal version numbers, on the other hand, requirements change, so the versioning schema changes accordingly and might become very complex.

Not realizing this difference between version numbers might introduce confusion and errors, and completely avoiding that users get in contact with internal version numbers could be a better choice.

Let’s dive a little more into the reasons a development group has to adopt a versioning system and into the different possibilities it faces.

First of all it is useful to state again that the need that drives the decision of labelling the produced software is that of being able to find quickly information about the production context. Such data change according to the development process, but I can list some common elements: author and date, hardware requirements, changes from the last version, potential compatibility issues or unresolved bugs.

The version number shall not necessarily contain such information directly. It can however encompass some of them, those data that the development team considers useful to be visible.

I think it is very wrong to give version number components a meaning of importance as it happens to external ones. Internal version numbers must represent a measurable quantity or precise steps of a procedure. Examples of measurable quantities are dates, a progressive number, the compatibility of some interfaces, the number of commits from the last version while examples of procedure steps are labels such as alpha, beta, test, if these labels have a clear meaning in the software development cycle.

For instance, an internal versioning schema could be the following

```
<date: YYYYMMDD_hhmmss>-<commits>
```

This way the version 20130803_092500-134 immediately tells the developer some important information, that is the date of creation, which makes it easily comparable to other versions, and the number of commits included from the last version, that can be used as a rough measure of the amount of work carried out.

Another example, taken from a software library versioning schema, is the following

```
<api>.<abi>.<code>
```

where the three numbers assume a very clear compatibility meaning. The part called `code` is a progressive number that shows changes that do not affect any interface. This encompasses changes like optimizations and changes in internal algorithms.

The central number (`abi`) signals a change in the binary interface (ABI, Application Binary Interface) that is it shows that changes happened that makes it necessary to recompile software that uses this library, without having to modify it. A typical example is given by the format change of a data type such as a structure. Function prototypes are the same as before, but an already compiled code would pass an incompatible data type.

The third and leftmost number (`api`) is linked to incompatibilities in the programming interface (API, Application Programming Interface). A classic example of API in a library is the set of all function prototypes. Changing the prototype of a function creates a big incompatibility issue, and this requires to adapt and recompile the softwares that use the library.

According to this schema a library moving from version `5.6.1` to version `5.6.2` may be installed without any further check while the subsequent release `6.0.0` is going to require a deep review of the release notes before installing it, perhaps requiring also the update of other software that uses it.

The two examples are very different since the two schemes try to solve different needs. Indeed, your mileage may vary according to your needs or the environment where you work. In my development team, for instance, a reduced version of the second example is used, which  consists of just the API and CODE numbers since the ABI break cases are so rare that can be handled as API breaks.

#### The dangerous major/minor system

An example of what I consider a dangerously ambiguous system is that proposed by Microsoft to .NET developers (http://msdn.microsoft.com/en-us/library/system.version.aspx). The schema is

```
<major>.<minor>.<build>.<revision>
```

where major is explicitly described with these words _“A higher version number might indicate a major rewrite of a product where backward compatibility cannot be assumed”_ while minor _“indicates significant enhancement with the intention of backward compatibility. This higher minor version number might indicate a point release of a product or a fully backward-compatible new version of a product”_.

What I consider dangerous in this description are the words _might_, _major rewrite_, _assumed_, _significant enhancement_, _intention_.

These words, indeed, express indefinite and unsteady concepts. A team that uses this schema without a previous specification is going to end up asking whether a given version with a new major number is incompatible or not, or if the changes made to some code are “major” or only “significant”.

The team that wants to use the .NET versioning system must first of all write a document where the meaning of such words and cases are clearly described, how to deal with incompatibilities, etc. Alas, the .NET system reflects one of the most widespread concepts about versioning, namely that versions have to do with the user's feeling about the changes, thus the major/minor nomenclature. User's perception of the changes in the software must be encompassed by an external number that should use a simpler schema.

This can never be stated enough: **internal version numbers must come from measurable quantities.**

## Versioning is a discipline

Versioning is not a set of fixed rules, but a **methodology**. When a development group follows a new methodology it both adopts and adapts it. Adopting means shifting the work procedures to follow those proposed by the methodology while adapting means changing the methodology to fit the environment where it is supposed to be used.

Versioning is a **discipline**, a way of thinking, where rules are the product of general concepts modelled after the scope and the environment where they are used. So the versioning scheme of a product can vary in the course of time since product or user requirements change.

Software is one of the environment where design is faster, always changing, pronged and distributed among several developers, so many things need to be versioned, not just the final product. This latter, indeed, could have a lifecycle of several years, but in the meantime bug fixes could be released, experimental code produced, tools and documentation enhanced. All those things can be, and many times they have to be versioned. **Always pay attention that the method for dealing with an object is imposed by the object itself, so the versioning scheme is imposed by the versioned object.** Your documentation probably needs a different schema from a software tool.

So the following guidelines must be shaped on the specific needs of the development group, and these are forced by the product, the size of the team, the market and several other surrounding conditions of which each team manager should take account.

1. **Versioning rules and procedures should take account not only of current needs but also of future ones**, to avoid as much as possible to be forced to change them. As usual in design it is recommended to consider the matter from several points of view and to explore different “what if?” scenarios. Doing this a team can both create a system that will last a long time and identify those cases where the system will fail. This way it is already known under which conditions the system will not satisfy the needs; those conditions, if forecasted, can be thus confronted with a clear change path in mind.

2. **Versioning rules must be clearly explained** and written in an easily accessible document (e.g. on an intranet in HTML format). Rules must be short. Use cases and frequently asked questions should be attached.

3. **Versioning rules must be quickly changed if they are no more suitable** for the use cases with which the team has to deal. One of the Extreme Programming sayings, “embrace change”, must be the spirit of any rule system which aims to be useful. That spirit integrates the first point given above since everyone can come across an unexpected condition and be in need of modifying the previous design choices.

4. **The change of versioning rules should be fully backward compatible if possible**. Every time the system evolves, it should thus become a superset of the previous rules. This way the technical shock originated by the change can be easily absorbed. Obviously, as already stated, there are many conditions in which backward compatibility is not possible. In such cases, the two systems should possibly be marked so that a version number is unequivocally connected to one of them (using a prefix, for example). The date when the new systems takes over the old one must be recorded.

## Finale

As you can see there is much to say about this subject, and this article only scratches the surface of the matter. However, I hope it helped you to have a big picture of the different aspects that hide behind those apparently simple labels or numbers called versions. Stay tuned for future articles on this topic through the [versioning](/categories/versioning/) tag.


