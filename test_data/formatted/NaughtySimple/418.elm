module X exposing (x)


x =
    "<<SCRIPT>alert("XSS");//<</SCRIPT>"
