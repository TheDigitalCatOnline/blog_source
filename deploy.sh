#!/bin/bash

source venv3/bin/activate

cd pelican
make publish
cd ..

rm -fR deploy/*
cp -R pelican/output/* deploy/

cd deploy
git add .
git ci -m "Updated"
git push
