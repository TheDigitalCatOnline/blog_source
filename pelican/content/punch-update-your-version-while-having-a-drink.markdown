Title: Punch - Update your version while having a drink
Date: 2016-05-31 16:00:00 +0100
Category: Projects
Tags: Git, Python, Python2, Python3, versioning
Authors: Leonardo Giordani
Slug: punch-update-your-version-while-having-a-drink
Image: punch
Summary: 

So you completed your wonderful new project, all your test are successful (you [test](/categories/tdd/) code, don't you?) and you just want to ship the new version and call it a day. Well, you just have to go and change the version number in your install script and save. Oh, right, you also have to open a feature branch, so that you may record the version update in your Git history. Well, easily done. Damn! You forgot to change the version number in the README.md file...
 
Managing the version number of a project is not easy. Not only you need to think about the versioning scheme and what part of the version to increase (see [this post]({filename}versioning-an-underrated-discipline.markdown) for some tips on this matter), but you also need to remember in which files you put the actual version number, and, depending on your workflow, to correctly manage the version control system commits.

**Punch** is a small tool that aims to simplify the latter parts, that is the management of the version number update process. Punch is a young project but the underlying structure should be flexible enough to allow the implementation of advanced usage cases that may arise in the future.

![Punch logo](/images/punch/icon_400x400.png)

#### Features

* Written in Python, but manages every kind of text file
* May implement different versioning schemas
* Currently supports integer version parts and multiple value integer parts
* Version parts may be explicitly set to given values
* The same version number can be represented in different ways using Jinja2 templates
* Each managed file may override the global behaviour with custom settings
* Can automatically commit using Git or git-flow
* May optionally create annotated commits
* Can simulate the version upgrade and show what is going to happen

#### Installation

You may install Punch in your virtual environment (or in your system) directly from PyPI

``` sh
pip install punch.py
```

and you will have the `punch` executable available.

#### Useful links

* Punch official documentation: [https://punch.readthedocs.io/en/latest/](https://punch.readthedocs.io/en/latest/)
* Official repository [https://github.com/lgiordani/punch](https://github.com/lgiordani/punch)
* Issues and pull requests [https://github.com/lgiordani/punch/issues](https://github.com/lgiordani/punch/issues)

Feel free to submit your issues or pull requests, contributions are welcome.