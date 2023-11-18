#!/bin/bash

markdown_tags=$(grep -h Tags pelican/content/*.markdown | sed -r -e s,"Tags: ",, -e s/", "/" "/g)
mau_tags=$(grep -h "pelican.tags:" pelican/content/*.mau | sed -r -e s,".*tags:",, -e s/", "/" "/g)

echo ${markdown_tags} ${mau_tags} | xargs -n 1 | sort | uniq | paste -d"," -s | sed s/","/", "/g
