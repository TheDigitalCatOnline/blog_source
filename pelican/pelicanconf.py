#!/usr/bin/env python
# -*- coding: utf-8 -*- #
from __future__ import unicode_literals

AUTHOR = "Leonardo Giordani"
SITENAME = "The Digital Cat"
SITESUBTITLE = "Adventures of a curious cat in the land of programming"
SITEURL = ""
DEBUG = True
# WEBASSETS_DEBUG = True # This unfortunately messes up the scrolling in the page

PATH = "content"

TIMEZONE = "Europe/Paris"

DEFAULT_LANG = "en"

# Feed generation is usually not desired when developing
FEED_ALL_ATOM = "atom.xml"
TAG_FEED_ATOM = "categories/{slug}/atom.xml"
CATEGORY_FEED_ATOM = "category/{slug}/atom.xml"
TRANSLATION_FEED_ATOM = None
DISPLAY_FEEDS_ON_SIDEBAR = True
DISPLAY_TAGS_ON_SIDEBAR = False

RELATED_POSTS_MAX = 10

JINJA_ENVIRONMENT = {"extensions": ["jinja2.ext.with_", "jinja2.ext.do"]}

ARTICLE_URL = "blog/{date:%Y}/{date:%m}/{date:%d}/{slug}/"
ARTICLE_SAVE_AS = ARTICLE_URL + "index.html"

CATEGORY_URL = "category/{slug}/"
CATEGORY_SAVE_AS = CATEGORY_URL + "index.html"

TAG_URL = "categories/{slug}/"
TAG_SAVE_AS = TAG_URL + "index.html"

ARCHIVES_URL = "archives/"
ARCHIVES_SAVE_AS = ARCHIVES_URL + "index.html"

AUTHOR_URL = "authors/{slug}/"
AUTHOR_SAVE_AS = AUTHOR_URL + "index.html"

PLUGIN_PATHS = ["../pelican-plugins"]
PLUGINS = ["related_posts", "series", "sitemap", "tag_cloud", "share_post", "webassets"]

SITEMAP = {
    "format": "xml",
}

DEFAULT_DATE_FORMAT = "%d/%m/%Y"

SOCIAL = (
    ("Twitter", "https://twitter.com/thedigicat"),
    ("GitHub", "https://github.com/TheDigitalCatOnline"),
)

DEFAULT_PAGINATION = 9
PAGINATION_PATTERNS = (
    (1, "{base_name}/", "{base_name}/index.html"),
    (2, "{base_name}/page/{number}/", "{base_name}/page/{number}/index.html"),
)

# Uncomment following line if you want document-relative URLs when developing
RELATIVE_URLS = True

STATIC_PATHS = [
    "images",
    "code",
    "notebooks",
    "files",
    "extra/CNAME",
    "extra/robots.txt",
]
EXTRA_PATH_METADATA = {
    "extra/CNAME": {"path": "CNAME"},
    "extra/robots.txt": {"path": "robots.txt"},
}

TWITTER_USERNAME = "thedigicat"

THEME = "../editorial"

FAVICON = "images/global/favicon.jpg"

MARKDOWN = {
    "extension_configs": {
        "markdown.extensions.codehilite": {"css_class": "highlight"},
        "markdown.extensions.extra": {},
        "markdown.extensions.meta": {},
        "markdown.extensions.toc": {"permalink": True},
        "mdx_video": {},
    },
    "output_format": "html5",
}


MAU = {
    "custom_templates": {
        "header.html": (
            '<h{{ level }} id="{{ anchor }}">'
            "{{ value }}"
            '{% if anchor and level <= 2 %}<a class="headerlink" href="#{{ anchor }}" title="Permanent link">¶</a>{% endif %}'
            "</h{{ level }}>"
        )
    },
    "pygments": {"html": {}},
}

QUOTES = [
    {
        "quote": "I've seen things you people wouldn't believe. Attack ships"
        " on fire off the shoulder of Orion. I watched C-beams glitter in the"
        " dark near the Tannhäuser Gate. All those moments will be lost in"
        " time, like tears in rain. Time to die.",
        "source": "Blade Runner, 1982",
    },
    {
        "quote": "Look at this. It’s worthless — ten dollars from a vendor in"
        " the street. But I take it, I bury it in the sand for a thousand"
        " years, it becomes priceless.",
        "source": "Raiders of the Lost Ark, 1981",
    },
    {
        "quote": "Roads? Where we're going, we don't need... roads.",
        "source": "Back to the Future, 1985",
    },
]
