#!/usr/bin/env bash
#
# ./myThesausrus.sh WORD1 [WORD2 [...]]
# 
#   needs: xmllint
#
URL="http://www.collinsdictionary.com/dictionary/english-thesaurus/"

for word in $@; do
  content=$(wget -qO- $URL$word)

  if [[ $content == *"Sorry, no results"* ]]; then
    tput bold
    tput setaf 3
    echo "No result!";
    tput sgr0
  else
    content=$(echo $content \
      | sed -n "/<body>/,/<\/body>/p" \
      | sed -e 's/<*img[^>]*>//g' \
      | sed -e 's/<a[^>]*>//g' \
      | sed -e 's/<\/a[^>]*>//g'\
      | xmllint --html --xpath "//div[@class='similar-words hom-subsec'][1]" -)
    tput bold
    tput setaf 6
    echo "$(echo $content \
      | xmllint --html --xpath "//h4/span" - \
      | sed -e 's/<[^>]*>//g')"
    tput sgr0

    number=$(grep -o "<li" <<< "$content" | wc -l)
    for (( i = 1; i <= $number; i++ )); do
      if [ -n "$(echo $content \
        | xmllint --html --xpath "//ol/li[$i]" -)" ]; then
        tput bold
        tput setaf 3
        echo "  $i"
        tput sgr0

        while IFS='|' read -ra ADDR; do
          for wrd in "${ADDR[@]}"; do
            echo "  * $(echo $wrd | sed 's/ *$//')"
          done
        done <<< "$(echo $content \
          | xmllint --html --xpath "//ol/li[$i]/span[@class='syn']" - \
          | sed -e 's/<\/span>/|/g' \
          | sed -e 's/<[^>]*>//g' \
          | sed -e 's/||/|/g' \
          | sed -e 's/ |/|/g' \
          | sed -e 's/| /|/g' \
          | sed -e 's/|$//g')"
      fi
    done
  fi
done
