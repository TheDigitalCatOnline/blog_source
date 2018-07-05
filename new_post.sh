#!/bin/bash

if [[ $# != 1 ]]; then echo "$0 <article-slug>"; exit 1; fi

source venv3/bin/activate
cd pelican/content

article_slug=$1
article_file=${article_slug}.markdown
touch ${article_file}

echo "Title: " > ${article_file}
echo "Date: $(date +"%Y-%m-%d %H:00:00 +0100")" >> ${article_file}
echo "Category:" $(grep -h "Category:" *.markdown | sed -r -e s,"Category: ",, -e s/", "/","/g | xargs -d "," -n1 | grep -Ev "^$" | sort | uniq | paste -d"," -s | sed s/","/", "/g
) >> ${article_file}
echo "Tags:" $(grep -h Tags *.markdown | sed -r -e s,"Tags: ",, -e s/", "/","/g | xargs -d "," -n1 | grep -Ev "^$" | sort | uniq | paste -d"," -s | sed s/","/", "/g
) >> ${article_file}
echo "Authors: Leonardo Giordani" >> ${article_file}
echo "Slug: ${article_slug}" >> ${article_file}
echo "Summary: " >> ${article_file}

echo "Created pelican/content/${article_file}"