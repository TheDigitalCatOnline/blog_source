Title: Punch 2.0.0 is out
Date: 2019-11-21 16:00:00 +0100
Category: Programming
Tags: Git, Python, Python3, versioning
Authors: Leonardo Giordani
Slug: punch-2-0-0
Image: punch_2_0_0
Summary: 

Punch 2.0.0 is out!

This is the latest release of the project that I started to replace bumpversion. Update your version while having a drink!

Punch is a configurable version updater, and you can use to automate the management of your project’s version number.

Changes:

* **DEPRECATION** Punch doesn't support GLOBAL variables in the FILES variable anymore. The values given to fields in the FILES section are now simple strings and are not processed through Jinja2 anymore.
* Initial drop of Python 2.x: the CI process doesn't test Python2.x anymore.
* Complete review of documentation: the docs have been split in multiple files to make it easier to find information and to understand the program.
* Initial implementation of automatic documentation from tests. Integration tests can now be parsed to extract examples for the documentation. See [Examples from the tests](https://punch.readthedocs.io/en/latest/test_examples/)
* Named serializers: serializers now can be given a name through a dictionary syntax. With this change it becomes possible to select the serializer to use for the VCS. See [Configuration > GLOBALS](https://punch.readthedocs.io/en/latest/configuration/#globals)
* Complex serializers: standard serializers use the same pattern both for the search and for the replace actions. With complex serializers you can define two different patterns, one for each action. See [Advanced configuration > Complex serializers](https://punch.readthedocs.io/en/latest/advanced/#complex-serializers)
* The configuration of each file managed by Punch can override the global serializers or add new ones. See [Configuration > FILES](https://punch.readthedocs.io/en/latest/configuration/#files)
* Release notes: Punch can be configured to check if a pattern based on the new version is present in the managed files. This makes it simple to check if HISTORY files have been updated without requiring to interrupt the execution of the program and later restore it. See [Advanced configuration > Release notes](https://punch.readthedocs.io/en/latest/advanced/#release-notes)

Read the full documentation [here](https://punch.readthedocs.io/en/latest/)