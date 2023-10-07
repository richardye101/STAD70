#!/bin/bash


cd book/
# fix image links in obsidian
find Notes/ -type f -name '*.md' -exec gsed -i 's+\!\[\[Obsidian-+\!\[\](Notes\/Obsidian-+g' {} \;
find ProblemSets/ -type f -name '*.md' -exec gsed -i 's+\!\[\[Obsidian-+\!\[\](ProblemSets\/Obsidian-+g' {} \;

find . -type f -name '*.md' -exec gsed -i 's+.png\]\]+.png)+g' {} \;


# build book 
# R --quiet -e "rmarkdown::render_site(encoding = 'UTF-8')"
R --quiet -e "rmarkdown::render_site(output_format = 'bookdown::gitbook', encoding = 'UTF-8')"
R --quiet -e "rmarkdown::render_site(output_format = 'bookdown::pdf_book', encoding = 'UTF-8')"

if [ $# -eq 0 ]
then
        echo "Build and Push"
        cd ../
        touch book/docs/.nojekyll
        rm -rf docs
        cp -r book/docs .
        git add .
        git commit -m "updated book"
        git push
else
        echo "Only build"
fi
