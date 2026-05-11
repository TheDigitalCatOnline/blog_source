#!/bin/bash

sass --style=expanded --no-source-map bengal/static/scss:bengal/static/css

cd pelican
pelican -lr $@
