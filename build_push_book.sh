#!/bin/bash


cd book/
# fix image links in obsidian
find Notes/ -type f -name '*.md' -exec gsed -i 's+\!\[\[Obsidian\/+!\[\](Notes\/Obsidian\/+g' {} \;
find Notes/ -type f -name '*.md' -exec gsed -i 's+.png\]\]+.png)+g' {} \;

# build book 
R --quiet -e 'rmarkdown::render_site(encoding = 'UTF-8')'
cd ../

touch book/docs/.nojekyll
rm -rf docs
cp -r book/docs .
git add .
git commit -m "updated book"
git push
