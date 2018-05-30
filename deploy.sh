#!/bin/bash

source venv3/bin/activate

cd pelican
pelican content -s publishconf.py
cd ..

rm -fR deploy/*
cp -R pelican/output/* deploy/

echo "Move to the deploy directory, check, commit and push"
