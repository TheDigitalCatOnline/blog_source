Title: libqgit2: a Qt wrapper for libgit2
Date: 2013-07-18 10:51 +0100
Category: Projects
Tags: Qt, C++, Git
Authors: Leonardo Giordani
Slug: libqgit2-a-qt-wrapper-for-libgit2
Summary:

[Libgit2](http://libgit2.github.com/) is "a portable, pure C implementation of the Git core methods" started in 2008 by Shawn O. Pearce. It is successfully used in both commercial an open source projects and wrapped in many languages (among the others Ruby, C#, Python). In 2011 Laszlo Papp from KDE started the libqgit2 project, aiming to port libgit2 features to C++/Qt. Sadly, lately the development slowed down a little and the library could not compile against the latest libgit2.

Being interested in using libgit2 in Qt/KDE projects, I am trying to restart the development.
I managed to update the library to libgit2 0.19.0 (latest version), and you can find the [code on GitHub](https://github.com/lgiordani/libqgit2). Feel free to fork it and work on it, but remember that this is just an unofficial repository.

Official repository is hosted by [KDE Projects](https://projects.kde.org/projects/playground/libs/libqgit2/repository/) and I'm managing to update it too.

At the moment, the library just wraps the main libgit2 objects and functions (git "plumbing").
Feel free to join me if you are interested; I'm not a Git guru, neither a Qt one, so any help is greatly appreciated.

Stay tuned for updates!