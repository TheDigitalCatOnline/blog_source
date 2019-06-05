#!/bin/bash

# This script sets up the blog development environment

if [[ ! -d venv ]]; then
    virtualenv venv3 -p python3
fi

source venv/bin/activate
pip install --upgrade pip
pip install -r requirements.txt

if [[ ! -d .git ]]; then
    git init
    git remote add origin git@github.com:TheDigitalCatOnline/blog_source.git
fi

if [[ -z $(git branch | grep develop) ]]; then
    git flow init -d
fi

if [[ ! -d pelican-plugins ]]; then
    git clone https://github.com/getpelican/pelican-plugins.git pelican-plugins
fi

if [[ ! -d pelican-themes ]]; then
    git clone https://github.com/getpelican/pelican-themes.git pelican-themes
fi

if [[ ! -d deploy ]]; then
    git clone git@github.com:TheDigitalCatOnline/TheDigitalCatOnline.github.com.git deploy
fi

