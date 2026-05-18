#!/bin/bash

./scripts/compile_scss.sh

cd pelican
pelican -lr $@
