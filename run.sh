#!/bin/bash

boussole compile

cd pelican
pelican -lr $@
