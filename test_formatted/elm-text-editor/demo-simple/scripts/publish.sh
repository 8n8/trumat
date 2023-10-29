color=`tput setaf 48`
reset=`tput setaf 7`

echo
echo "${color}:Publishing ...${reset}"

SOURCE=/Users/carlson/dev/elm/mylibraries/elm-text-editor/demo-simple/src
PUBLIC=/Users/carlson/dev/elm/mylibraries/elm-text-editor/demo-simple/public
TARGET=/Users/carlson/dev/github_pages/app/editor-simple

elm make --optimize ${SOURCE}/DemoSimple.elm --output=${PUBLIC}/DemoSimple.js

echo
echo "${color}:Uglifying ...${reset}"

uglifyjs ${PUBLIC}/DemoSimple.js -mc 'pure_funcs="F2,F3,F4,F5,F6,F7,F8,F9"' -o ${PUBLIC}/DemoSimple.min.js

echo
echo "${color}:Copying ...${reset}"

cp ${PUBLIC}/index-remote.html ${TARGET}/index.html
cp ${PUBLIC}/DemoSimple.min.js ${TARGET}/


echo
echo "${color}cd /Users/carlson/dev/github_pages${reset}"


cd /Users/carlson/dev/github_pages
