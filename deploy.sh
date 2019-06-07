#!/bin/bash

source venv3/bin/activate

cd pelican
make publish
cd ..

rm -fR deploy/*
cp -R pelican/output/* deploy/

echo "Move to the deploy directory, check, commit and push"
