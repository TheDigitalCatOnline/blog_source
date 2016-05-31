Title: Punch - Update your version while having a drink
Date: 2016-05-31 16:00:00 +0100
Category: Projects
Tags: Git, Python, Python2, Python3, versioning
Authors: Leonardo Giordani
Slug: punch-update-your-version-while-having-a-drink
Summary: 

So you completed your wonderful new project, all your test are successful (you [test](/categories/tdd/) code, don't you?) and you just want to ship the new version and call it a day. So you just have to go and change the version number in your install script and save. Oh, right, you also have to open a feature branch, so that you may record the version update in your Git history. Well, easily done. Damn! You forgot to change the version number in the README.md file...
 
Managing the version number of a project is not easy. Not only you need to think about the versioning scheme and what part of the version to increase (see [this post](/blog/2013/03/20/versioning-an-underrated-discipline/) for some tips on this matter), but you also need to remember in which files you put the actual version number, and, depending on your workflow, to correctly manage the version control system commits.

Punch is a small tool that aims to simplify the latter parts, that is the management of the version number update process. Punch is a young project but the underlying structure should be flexible enough to allow the implementation of advanced usage cases that may arise in the future.

In this post I want to explain how to use Punch and review some use cases. I will also give a brief overview of the internal workflow, so that you may better understand how Punch works to manage the version of your project.

** This post refers to Punch v.1.2.0 **

## Install

You may install Punch in your virtual environment (or in your system) directly from PyPI

``` sh
pip install punch.py
```

and you will have the `punch` executable available.

## Initialization

Punch needs two configuration files to run, both written in pure Python. The first file is the actual _config file_ and its default name is `punch_config.py`, while the second is called _version file_ with a standard name of `punch_version.py` and contains only the value of the current version. Since both files are written in pure Python you are allowed to put in them any valid Python code. Be aware, however, that the version file will be overwritten when the version is updated, so it is useless to modify it.
  
To create two initial files you just need to run

``` sh
punch --init
```

and punch will write the `punch_config.py` and `punch_version.py` files in the current directory. Punch will not overwrite any previously existing file.

The standard configuration implements a `major.minor.patch` version scheme as described by the [semantic versioning specification](http://semver.org) without the optional prerelease and build information. The standard version written in the version file is `0.1.0`. The config file, however, is not configured to manage any file, obviously. 
 
If you are already using this schema you just need to edit the `punch_version.py` file putting the actual version number of your project, and then edit the `punch_config.py` file and fill the `FILES` variable with the files that contain your version number.

### Example of initialization

Say that my CoolStuff project's version is `1.4.5` and that the number is in the `README.md` file and in the `docs/index.md` file. The relevant section of the `README.md` file is the following

``` markdown
* Current version: 1.4.5
```

while the `docs/index.md` file contains the following code

``` markdown
Last version is 1.4.5
```

The `punch_version.py` file created by Punch contains the following Python lines

``` python
major = 0
minor = 1
patch = 0
```

and the `punch_config.py` file contains

``` python
__config_version__ = 1

GLOBALS = {
    'serializer': '{{major}}.{{minor}}.{{patch}}',
}

FILES = []

VERSION = ['major', 'minor', 'patch']

VCS = {
    'name': 'git',
    'commit_message': "Version updated from {{ current_version }} to {{ new_version }}",
}
```

We just need to edit the two files with the correct values from the CoolStuff project. The `punch_version.py` files becomes

``` python
major = 1
minor = 4
patch = 5
```

and the `punch_config.py` file becomes

``` python
__config_version__ = 1

GLOBALS = {
    'serializer': '{{major}}.{{minor}}.{{patch}}',
}

FILES = ['README.md', 'docs/index.md']

VERSION = ['major', 'minor', 'patch']
```

Note that I removed the `VCS` variable. The default configuration file instructs Punch to use Git and to automatically commit any version change, but this behaviour shall be configured to fit your workflow, so by now we will avoid it.

## Simulating execution

At any time you may specify the `--simulate` or `-s` switch and the Punch executable will print a detailed report of the changes that will be applied without affecting any file. 

## Using different config files

You may change the name of the config file and the version file and specify the new names with the two `--config-file`/`-c` and `--version-file`/`-v` switches.  

## Basic usage

As you can see, the `VERSION` variable of the config file lists the version _parts_ with their names. When Punch is configured you may at any time run it with the `--part` switch followed by a part name, and Punch will increase the value of that part, changing the other parts accordingly.

For example, with the configuration shown above, issuing the command

``` sh
punch --part patch
```

will update the version to `1.4.6`. The values of the three parts will be written in the `punch_version.py` file and the whole version will be written both in the `README.md` and in the `docs/index.md` files.

Starting again with the above configuration, if you run the command
 
``` sh
punch --part minor
```
 
Punch will update the version to `1.5.0`, that is _increases_ the `minor` part from 4 to 5 and _resets_ the `patch` part to 0. Finally, running the command on the `major` part would produce the `2.0.0` version, thus _increasing_ the `major` part and resetting the following ones.

This behaviour is provided by the `VERSION` variable. When Punch is requested to increase one of the parts it also resets every part listed _after_ the changed one (that is, listed on its right in the list).

## Advanced usage

If you need to explicitly set a part skipping some numbers you may use the `--set-part` switch. With the given initial configuration you may run
 
``` sh
punch --set-part minor=7
```

and you will obtain the version `1.7.6`.

Note that setting a part does not reset the following ones. If you want to force that behaviour you have to specify the `--reset-on-set` switch.

``` sh
punch --set-part minor=7 --reset-on-set
```

will produce the version `1.7.0`.

The `--set-part` switch may be applied to more than one part, listing all of them separated by commas

``` sh
punch --set-part minor=7,patch=12
```

but in this case you cannot use the `--reset-on-set` switch as it would be meaningless.