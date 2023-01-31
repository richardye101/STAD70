#!/bin/bash
touch book/docs/.nojekyll
rm -rf docs
cp -r book/docs .
git add .
git commit -m "updated book"
git push
