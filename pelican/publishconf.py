#!/usr/bin/env python
# -*- coding: utf-8 -*- #
from __future__ import unicode_literals

# This file is only used if you use `make publish` or
# explicitly specify it as your config file.

import os
import sys
sys.path.append(os.curdir)
from pelicanconf import *

SITEURL = 'http://lgiordani.com'
RELATIVE_URLS = False

FEED_ALL_ATOM = 'atom.xml'
TAG_FEED_ATOM = 'categories/%s/atom.xml'
CATEGORY_FEED_ATOM = 'category/%s/atom.xml'

DELETE_OUTPUT_DIRECTORY = True

DISPLAY_FEEDS_ON_SIDEBAR = True
DISPLAY_TAGS_ON_SIDEBAR = False

# Following items are often useful when publishing

#DISQUS_SITENAME = ""
#GOOGLE_ANALYTICS = ""
