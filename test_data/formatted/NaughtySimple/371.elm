module X exposing (x)


x =
    "<img \x00src=x onerror="javascript:alert(1)">"
