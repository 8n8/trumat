module X exposing (x)


x =
    "perl -e 'print "<IMG SRC=java\0script:alert(\"XSS\")>";' > out"
