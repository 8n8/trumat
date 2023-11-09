module Strings exposing (about, changeLog, lesson, markdownExample, mathExample, test)


test2 =
    """
Test

$$
a^2 = 1
$$

Comments
"""


testxx =
    """Test

$$
a^2 = 1
$$

Note also that there is an automatically
generated active table of contents.
It can be placed inside the document
at the top, to one side, as it is
here, or it can be absent.

Comments   
"""


test =
    """
## A Pure Elm Text Editor

You are looking at an experimental text
editor written in pure Elm.  In this
demo, you can enter and edit text
in Markdown + Math: normal Markdown,
with mathematical text between dollar
signs and double dollar signs.  Thus we
can write the Pythagorean formula,
$a^2 + b^2  = c^2$, and a familiar
integral:

$$
\\int_0^1 x^n dx = \\frac{1}{n+1}
$$

Click on other tabs below for info
about the editor and for more
examples.

**Editor commands.**   For
a list of these, use
ctrl-h.  Typing ctrl-h again
hides the list and restores
the original text.  Some commands:
ctrl-z to undo, ctrl-y to redo,
ctrl-z to search, ctrl-r for
search/replace. To sync the
source and rendered texts,
click somewhere in the editor
and type ctrl-shift-s.  This
feature still needs some work.

**Notes**

- All this is work in progress:
Ready for experimentation but not production.

- The app you see here is in general
ahead of the published package (jxxcarlson/elm-text-editor).


"""


mathExample =
    """
# Propagation and Evolution


## The propagator

Consider a wave function $\\psi(x,t)$.
If we fix $t$ and let $x$ vary, the
result is an element $\\psi(t)$ of
$L^2(R)$ or, more generally
$L^2(\\text{configuration space})$.
Thus the evolution of our system in
time is given by a function
$t \\mapsto \\psi(t)$.  The dynamics
of this path in Hilbert space is
governed by an ordinary differential
equation ,

$$
i\\hbar\\frac{d\\psi}{dt} = H\\phi,
$$

Now consider bases of orthogonal
normalized states
$\\{\\; \\psi_k(t_1)\\;\\}$ and
$\\{\\; \\psi_k(t_0) \\; \\}$
at times $t_1$ and $t_0$,
with $t_1 > t_0$. There is a unique linear
transformation $U(t_1,t_0)$
such that
$\\psi_k(t_1) = U(t_1,t_0)\\psi_k(t_0)$
for all $k$.
It must be unitary because the bases
are orthonormal.
This family of transformations is
called the \\term{propagator}.
The propagator satisfies various
identities, e.g., the composition law

$$
U(t_2, t_0) = U(t_2, t_1)U(t_1, t_0)
$$

as well as $U(t,t) = 1$,
$U(t_1,t_2) = U(t_2,t_1)^{-1}$.

Let us write $U(t) = U(t,0)$ for
convenience, and let us suppose given
states $\\alpha$ and $\\beta$.
The probability that the system finds
itself in state $\\beta$ after time $t$
is given by the matrix element

$$
\\bra \\beta U(t)  \\ket \\alpha
$$

This is just the kind of information
we need for comparison with experiment.

The propagator, like the family of state
vectors $\\psi(t)$, satisfies a differential
equation -- essentially a Schroedinger equation
for operators. To find it, differentiate the
equation $\\psi(t) = U(t)\\psi(0)$ to obtain

$$
i\\hbar \\frac{d\\psi}{dt} = i\\hbar \\frac{dU}{dt}\\psi(0)
$$

Substitute (C) to obtain

$$
i\\hbar \\frac{dU}{dt}\\psi(0)  = H\\psi(t)
$$

Applying $\\psi(t) = U(t)\\psi(0)$ again, we find that

$$
i\\hbar \\frac{dU}{dt}\\psi(0) = HU\\psi(0)
$$

If this is to hold for arbitrary $\\psi(0)$, then

$$
\\frac{dU}{dt} = -\\frac{i}{\\hbar}HU
$$

If $H$ does not depend on time, the preceding
ODE has an immediate solution, namely


$$
U(t) = e^{-i(t/\\hbar) H}
$$

Think of $H$ as a big matrix, and of the expression
on the right as a big matrix exponential.


## Notes on the free-particle propagator

Below are graphs of the real part of the
free-particle propagator for time
$t = 1, 2, 4,16$.


![xx::center](http://noteimages.s3.amazonaws.com/jim_images/propagator-t=1-63c8.png)


![xx::centerhttp://noteimages.s3.amazonaws.com/jim_images/propagator-t=2-6feb.png)

![xx::center](http://noteimages.s3.amazonaws.com/jim_images/propagator-t=4-a035.png)

![xx::center](http://noteimages.s3.amazonaws.com/jim_images/propagator-t=16-e5ae.png)

**Jupyter code**

```python
%matplotlib inline

import matplotlib.pyplot as plt
import numpy as np

x = np.linspace(0, 6*np.pi, 500)
t=4
plt.plot(x, np.cos(x**2/t)/np.sqrt(t))
plt.title('Free particle propagator, t=4');
```

"""


changeLog : String
changeLog =
    """
 # Main Issues

- Gutter numbers do not align with true line numbers when there are
very long lines.

- Synchronized scrolling of editor and rendered text windows

- Auto-wrap as text approaches right-hand margin


See the [full list of issues](https://github.com/jxxcarlson/elm-text-editor/issues).
I may need help on some of these.

 ## Fixed

 - Jump when user clicks at end of line (Fixed Jan 14, 2020).  Thanks to
 Martin Stewart and Wolfgang Schuster whose precise error reports
 helped fix this.

 # ChangeLog

 ## January 19, 2020

 - Replace funky slider with real scrolling
 
 ##  January 13, 2020 (version 5.0.0)
 
 - Change **Clear All** command (ctrl-shift-C) to (ctrl-option-C)
 
 - Add feature *Copy editor selection to system clipboard*, attach 
 to keyboard command ctrl-shift-C.

 ## January 12, 2020 (version 4.0.0)

 - Simpler API for embedding edtior

 - Configure app using height of editor window

    """


about : String
about =
    """
## About the Pure Elm Text Editor Project

![Hummingbird::left](http://noteimages.s3.amazonaws.com/jxxcarlson/hummingbird2.jpg)
This project
grew out the need for a
pure Elm text editor.  It is
very much a work in progress, for
which reason there will be a higher than
the usual frequency of updates. Comments,
issues, and pull requests are welcome:
I would like the editor to become a
generally useful tool in addition to one that
I use in my own projects. Please write me at
jxxcarlson at gmail with comments and bug
reports, or (better yet) post an issue on the
[GitHub repo](https://github.com/jxxcarlson/elm-text-editor).

A text editor is useful to the extent that it
can work as part of other applications. To this
end,
we show how it an be used with a
[pure Elm Markdown](https://github.com/jxxcarlson/elm-markdown)
parser-renderer.  The renderer
uses [pablohirafuji/elm-syntax-highlight](https://package.elm-lang.org/packages/pablohirafuji/elm-syntax-highlight/latest/). Language
support at this time consists of Wlm,
Javascript, Xml, Css, Python, Sql, and Json.

**NOTE.** This app is generally a few steps ahead of the version in the
published package.

## History

I first learned about the possibility of a pure Elm text editor
from the 2018 Discourse post of [Martin Janiczek](https://discourse.elm-lang.org/t/text-editor-done-in-pure-elm/1365/8),
which I must have read in that same year.  [Janiczek's code](https://github.com/Janiczek/elm-editor) provides (1) a
foundational editor structure with facilities for inserting and deleting characters
and mving the insertion point around with arrow keys, (2) a set of fuzzer tests to
verify the good operation of the editor.  These property-based tests use
his [Architecture Test](https://package.elm-lang.org/packages/Janiczek/architecture-test/latest/) package. (I am working slowly to implement such tests for this project).


A few day before the end of 2019, I came across
wonderful work of [Sydney Nemzer](https://sidneynemzer.github.io/elm-text-editor/), who
had also commented on the Discourse post.  [Nemzer's code](https://github.com/SidneyNemzer/elm-text-editor),
which greatly extends Janiczek's is the basis for what
you see here, and with it rapid progress was possible. It is a pleasure to
acknowledge the works on which this package is built.




## Some features

Most of the features are documented (after a fashion)
in the help panel, which you can access by typing ctrl-h
in the editor.  Typing ctrl-h again closes the the help
panel.  Here are some notable features:

- **External copy-paste.** Do cmd-C (or whatever) to copy
text outside the editor (wherever).  Then click in
the editor and do ctrl-shift-V to paste the text
in.  In the reverse direction, use cmd-shift-C to copy
editor text to the system clipboard.  At the moment, these
are the only feature which
use ports. Also, these features currently work only
in Google Chrome.  Not good!

- **Sync with rendered text (Left to Right).**  Do ctrl-shift-S in the
editor to raise the source and rendered text to
(more or less) the same height near the top of the
respective windows.  This feature needs a lot
more more work.  To implement it, one searches the AST
of the source text to find the id of the rendered text in question. *((This feature is not yet reliable. The
searchAST function needs more thought.  I'm working on it.))*

- **Wrapping Text.**  ctrl-W wraps the current selection.  ctrl-shift-W
wraps the entire document respecting paragraphs, code blocks,
and LaTeX blocks, e.g. `$$ a^2 + b^2 = c^2 $$.` In this app, for simplicity,
we do not implement math rendering. See [markdown.minilatex.app](https://markdown.minilatex.app/). Still to do: protect Markdown tables and LaTeX begin-end pairs.

- **Debounce.** Debounce is built into the editor and can
be used or not used as needed in the host app. If you
wish to use it, simply handle the `Unload` message from
the editor however you like and ignore the `Insert` message.  The debounce interval
is currently set at 100 ms.  We use the package
[jinjor/elm-debounce/](https://package.elm-lang.org/packages/jinjor/elm-debounce/latest/).


## Some missing features

There are many.  But here are some that particularly
interest me:

- Right to Left editor sync: select rendered text to sync
with corresponding source text.

- Linked scrolling of source and rendered text.


"""


markdownExample : String
markdownExample =
    """
# Markdown Demo

The Markdown in this window is rendered using the Elm package
[jxxcarlson/elm-markdown](https://github.com/jxxcarlson/elm-markdown). Below we illustrate some typical Markdown
elements: images, links, headings, etc.

![Hummingbird](http://noteimages.s3.amazonaws.com/jxxcarlson/hummingbird2.jpg)
Hummingbird (Meditation)

Link: [New York Times](http://nytimes.com)

Text styles: **bold** *italic* ~~strike it out~~



## Code

He said that `a := 0` is an initialization
statement.

```python
# Partial sum of the harmonic series:

sum = 0
for n in range(1..100):
  sum = sum + 1.0/n
sum
```

## Verbatim and Tables (Extensions)

A verbatim block begins and ends
with four tick marks. It is just
like a code block, except that there is no
syntax highlighting.  Verbatim blocks
are an extension of normal Markdown.

````
Verbatim text has many uses:

   Element    |    Z
   --------------------
   Altium     |    4/5
   Brazilium  |    7/5
   Certium    |    9/5
````

But better is to use Markdown tables:

|  Element  | Symbol |  Z | A |
| Hydrogen  | H      |  1 | 1.008   |
| Helium    | He     |  2 |  4.0026 |
| Lithium   | Li     |  3 |  6.94   |
| Beryllium | Be     |  4 |  9.0122 |
| Boron     | B      |  5 | 10.81   |
| Carbon    | C      |  6 | 12.011  |
| Nitrogen  | N      |  7 | 14.007  |
| Oxygen    | O      |  8 | 15.999  |
| Flourine  | F      |  9 | 18.998  |
| Neon      | Ne     | 10 | 20.180  |


## Lists

Indent by four spaces for each level.  List items
are separated by blank lines.

- Solids

    - Iron *(metal)*

        - Iron disulfide (Pyrite): $FeS_2$, crystalline

        - Iron(II) sulfed $FeS$, not stable, amorphous

    - Selenium *(use for solar cells)*

- Liquids

    - Alcohol *(careful!)*

    - Water *(Ok to drink)*


## Quotations


Quotations are offset:

> Four score and seven years ago our
fathers brought forth on this continent,
a new nation, conceived in Liberty,
and dedicated to the proposition
that all men are created equal.

> Now we are engaged in a great c
ivil war, testing whether that
nation, or any nation so
conceived and so dedicated,
can long endure. We are met o
In a great battle-field of that war.
We have come to dedicate a portion
of that field, as a final resting
place for those who here gave their
lives that that nation might live.
It is altogether fitting and proper
that we should do this.

> But, in a larger sense, we can not
dedicate — we can not consecrate —
we can not hallow—this ground. The brave men,
living and dead, who struggled here,
have consecrated it, far above our poor
power to add or detract. The world will
little note, nor long remember what we say
here, but it can never forget what they d
id here. It is for us the living, rather,
to be dedicated here to the unfinished
work which they who fought here have thus
far so nobly advanced. It is rather for
us to be here dedicated to the great task
remaining before us—that from these
honored dead we take increased devotion
to that cause for which they gave the
last full measure of devotion—that we
here highly resolve that these dead
shall not have died in vain—that
this nation, under God, shall have
a new birth of freedom—and that
government of the people, by the people,
for the people, shall not perish
from the earth.

— Abraham Lincoln, *Gettysbug Address*

## Poetry (Extension)

Poetry blocks, an extension of normal Markdown,
 begin with ">>"; line endings are respected.

>> Twas brillig, and the slithy toves
Did gyre and gimble in the wabe:
All mimsy were the borogoves,
And the mome raths outgrabe.

>> Beware the Jabberwock, my son!
The jaws that bite, the claws that catch!
Beware the Jubjub bird, and shun
The frumious Bandersnatch!


Etcetera!

___


NOTE: this Markdown implementation is
an option for writing documents on
[knode.io](https://knode.io).
Knode also offers MiniLaTeX,
a web-friendly subset of TeX/LaTex.
To see how it works without a sign-in, please
see [demo.minilatex.app](https://demo.minilatex.app).


___

## Installation


To compile, use

```elm
elm make --output=Main.js
```

Then open `index.html` to run the app.


"""


lesson =
    """
# A Counter App

___

Source: [Intro to Elm](https://math-markdown.netlify.com/#doc/jxxcarlson.intro-elm.3a00)

___


![counter-app::left](http://noteimages.s3.amazonaws.com/jim_images/counter-app-yy.png)

We begin by building the very simple Elm app, pictured
on the left. Tahe app has three buttons and a display
for a counter. The buttons can increment the counter,
decrement it, or reset its value to zero. We will
start with the version of the app from the [Elm
Guide](https://guide.elm-lang.org), then elaborate
it slightly, adding some styling and the reset button.


The app we will build provides a pattern that we will
follow for other apps. Every app will have a Model,
in which the data for the app is stored. The model for
the Counter App model consists of a single integer.
Every app also has a `view` function, which tranforms
the data in the model into `Html` data that specifies
the appearance of a web page. That data is sent to
the "Elm Runtime," which is a Javascript machine
that knows how to make web pages from `Html` data.
The data flow from Elm to its Runtime is represented
by the top arrow in the figure below.


The constructed web page may contain text, images,
and active elements like buttons, as in the counter
appelm above left. When the user presses one of the
buttons, a message is sent to the Elm program. For
example, pressing "+" sends a message to increase the
value of the model by 1, while pressing "-" sends a
message to decrease it by 1. The `update` function
uses the message and the current value of the model
to compute a new model, completing the cycle pictured
below. The data flow from the Runtime to Elm is
represented by the bottom arrow.


![cc](https://guide.elm-lang.org/effects/diagrams/sandbox.svg)

Let's now get to work!

## 1. Try the code


> [Code from the Elm Guide](https://guide.elm-lang.org/architecture/buttons.html)

Press **Edit** to run the app.

Experiment with the app.  What does it do?

## 2. Add some space around the app

Add this line:

```elm
import Html.Attributes as HA
```

and then change

```elm
view model =
 div []
   ...
```

to

```elm
view model =
 div [HA.style "padding" "40px"]
   ...
```

Press **Compile** to run the app with these changes.


## 3. Make the buttons look better

Let's change the appearance of the first button:

```elm
[ button [   HA.style "height" "30px"
          , HA.style "font-size" "18px"
          , onClick Decrement ] [ text "-"
         ]
```

### To do

- Make a correponding change in the second button.

- Adjust the appearance of the counter. Things to
try: "padding-top", "padding-left"

## 4. Add a Reset Button

What you will need to do:

1. Add a new button that sends a `Reset` message
when it is clicked.

1. Add `Reset` to `Msg`

2. Add a new clause to the `update` function that defines what happens when the `Reset` message is received.

This is the recipe that you follow whenever you add a new action to your app.







## 5. Studying the Code

Let's study the [code for the basic app](https://elm-lang.org/examples/buttons).  It gives us a pattern to follow for making other Elm apps.


We will comment line by line.  You should read through this part twice, because sometimes an earlier thing makes more sense in light of a later thing.

**1.**  Declare the name of the module.  A module is a collection of functions and values.  The fragment `exposing(..)` determines what is visible outside the module.  If we write `(..),` everything defined in the module is visible outside.  If we had written `(main),` then only the function `main` would be visible.

```elm
module Main exposing (..)
```


**10-12.** Import other modules

**19-20.** The function `main`

```elm
main = Browser.sandbox {  init = init
                       , update = update
                       , view = view }
```

The `Browser` module defines different kinds of Elm apps.  The `sandbox` app is the simplest. To define a sandbox app, we must define three functions. First is `init,` which sets up the initial model for the app.  The model is where the app's data is stored.  Second is `update.`  It computes a new model given the old model and whatever messages the app has received.  For example, if the user had pressed the "+" button, the `Increment` message will have been received.  The `view` function computes a new web page based on the information contained in the model.


**27. The model**

```elm
type alias Model = Int
```

The model is just an integer.  It holds the value of our imaginary counter.  In Elm, everything has a *type*, about which we will have to say more later.  In this case, the model has type `Int` —\u{00A0}Int for integer.

**30-32. The init function**

```elm
init : Model
init = 0
```

All we do here is set the value of the model to zero.  The first line defins the type of `init.`  The second line defines the value of init.  Althought types can be omitted, it is good practice to use them throughout your code.  You will get better compiler messages, and you will find that your code is easier to undertand when you come back to it after some time has passed.

**39-41. Msg**

```elm
type Msg
 = Increment
 | Decrement
```

Define the type of messages that the app knows how to handle.

**44-51. The update function**

```elm
update : Msg -> Model -> Model
update msg model =
 case msg of
   Increment ->
     model + 1

   Decrement ->
     model - 1
```

Note the first line:

```elm
update : Msg -> Model -> Model
```

This is the *type signature* for the `update` function.
The type signature tells us what kind of inputs a
function needs in order to calculate its output, and
it also tells us what kind thing the output is. Here
the type signature says that `update` takes a message
and a model as input produces a model as output.

The `case` statement gives a way of handling the
different messages that can be received. If the
message is `increment,` we return the old value of
the model plus one. If the message is `decrement,`
we return the old value of the model minus one.


**58-61. The view function**

```elm
view : Model -> Html Msg
view model =
 div []
   [ button [ onClick Decrement ] [ text "-" ]
   , div [] [ text (String.fromInt model) ]
   , button [ onClick Increment ] [ text "+" ]
```

Look at the type signature of the view function:

```elm
view : Model -> Html Msg
```

It says that the function takes the model as input and returns a value of type `Html Msg` as output.  That is, it constructs the data needed to make a web page.


## 6. Inside the view function

Everything in Elm is either value, like the integer 43,
or function.  We can figure out what kind of a thing `div` and `button` are by looking up the [documentation](https://package.elm-lang.org/packages/elm/html/latest/Html#div) or by using the repl.  Either way, we get this:

```elm
div :    List (Attribute msg) -> List (Html msg) -> Html msg
button : List (Attribute msg) -> List (Html msg) -> Html msg
```

These functions take lists of things as arguments and return somethig of type `Html msg,` which is the type of data needed to construct a web page. Let's look at the `button` function as it occurs in the `view` function:

```elm
button [ onClick Decrement ] [ text "-" ]
```

The first argument is a list of things of type `Attribute msg.`  In the case at hand, there is just one such thing, `onClick Decrement.`  So what is `onClick?` If we look up its type in the [docs](https://package.elm-lang.org/packages/elm/html/latest/Html-Events#onClick), we find this:

```elm
onClick: msg -> Attribute msg
```

Thus `onClick` transforms a `msg` into an `Attribute msg,` which is just what we need.  Here is an example in which the first argument is a list of two elements:

```elm
button [ onClick Decrement, style "width" "30px" ] [ text "-" ]
```

And we find that `style` is a function that takes two strings as input and produces a value of type `Attribute msg` as output.

```elm
style : String -> String -> Attribute msg
```

To complete the picture, we consder the second argument of `button,` namely the expression `text "-".`. Once again, `text` is a function:

```elm
text: String -> Html msg
```

Thus `text` tranforms a string into a value of type
`Html msg.` Again the types fit, since the second
agument of `button` has type `List (Html msg).`




## 7. Functions in Elm

Elm is a language of *pure functions,* meaning that the only thing a function can do is to compute an output value, where that value depends on the inputs and *absolutely nothing else.* This means that if you give a function the same inputs at different times, you get the same output, just as in mathematics.  We cannot have $2 + 2 = 4$ one time and $2 + 2 = 5$ another.  As a real counterexample, consider this Javascript example:

```elm
> Math.random()
0.5593930691704239

> Math.random()
0.636367324500821
```

The `Math.random()` function is not pure, because it gives different values given the same input.

Programs made of pure functions are easier to debug and reason about than programs which are not pure.  To know what a function does, it is enough to understand its code and to know the inputs.  We do not have to know the execution history of the program.

**Note.** Purity does not mean that we cannot use random numbers in Elm.  We just have to do it in a different way (using *commands*).  See the section on building a Quotes App, where we use a random number generator to display random quotes to the user.



## Final version


Here is the code for the [final version](https://ellie-app.com/7wxGhBTvpTqa1).
       
"""
