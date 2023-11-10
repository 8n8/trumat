module AppText exposing (gettysburgAddress, jabberwocky, long, longLines, tolstoy)


gettysburgAddress =
    """
Below is Abraham Lincoln's Gettysburg Address.
It was loaded as three long lines.  This example
illustrates the current state of the text-wrap
functionality.  It is based on Folkert de Vries'
elm-paragraph library.

Four score and seven years ago our fathers brought forth on this continent, a new nation, conceived in Liberty, and dedicated to the proposition that all men are created equal.
Now we are engaged in a great civil war, testing whether that nation, or any nation so conceived and so dedicated, can long endure. We are met on a great battle-field of that war. We have come to dedicate a portion of that field, as a final resting place for those who here gave their lives that that nation might live. It is altogether fitting and proper that we should do this.

But, in a larger sense, we can not dedicate—we can not consecrate—we can not hallow—this ground. The brave men, living and dead, who struggled here, have consecrated it, far above our poor power to add or detract. The world will little note, nor long remember what we say here, but it can never forget what they did here. It is for us the living, rather, to be dedicated here to the unfinished work which they who fought here have thus far so nobly advanced. It is rather for us to be here dedicated to the great task remaining before us—that from these honored dead we take increased devotion to that cause for which they gave the last full measure of devotion—that we here highly resolve that these dead shall not have died in vain—that this nation, under God, shall have a new birth of freedom—and that government of the people, by the people, for the people, shall not perish from the earth."""


tolstoy =
    """“Well, Prince, so Genoa and Lucca are now just family
estates of the Buonapartes. But I warn you, if you don’t
tell me that this means war, if you still try to defend
the infamies and horrors perpetrated by that Antichrist—
I really believe he is Antichrist—I will have nothing
more to do with you and you are no longer my friend,
no longer my ‘faithful slave,’ as you call yourself!
But how do you do? I see I have frightened you—
sit down and tell me all the news.
 ”"""


longLines =
    """
Bioluminescence might seem uncommon, even alien. But biologists think organisms evolved the ability to light up the dark as many as 50 different times, sending tendrils of self-powered luminosity coursing through the tree of life, from fireflies and vampire squids to lantern sharks and foxfire, a fungus found in rotting wood.

Despite all this diversity, the general rules stay the same. Glowing in the dark or the deep takes two ingredients. You need some sort of luciferin, a molecule that can emit light. And you need an enzyme, luciferase, to trigger that reaction like the snapping of a glowstick.

Some creatures delegate this chemistry to symbiotic bacteria. Others possess the genes to make their own versions of luciferin and luciferase. But then there’s the golden sweeper, a reef fish that evolved a trick that hasn’t been seen anywhere else, according to a study published Wednesday in Science Advances: It just gobbles up bioluminescent prey and borrows the entire kit.

“If you can steal an already established, sophisticated system by eating somebody else, that’s way easier,” said Manabu Bessho-Uehara, a postdoctoral scholar at the Monterey Bay Aquarium Research Institute.

```elm
update : Buffer -> Msg -> InternalState -> ( InternalState, Buffer, Cmd Msg )
update buffer msg state =
    case msg of
        NoOp ->
            ( state, buffer, Cmd.none )

        FirstLine ->
            let
                cursor =
                    { line = 0, column = 0 }

                window =
                    Window.scrollToIncludeCursor cursor state.window
            in
            ( { state | cursor = cursor, window = window, selection = Nothing }, buffer, Cmd.none ) |> recordHistory state buffer

        AcceptLineNumber nString ->
            case String.toInt nString of
                Nothing ->
                    ( state, buffer, Cmd.none )

                Just n_ ->
                    let
                        n =
                            clamp 0 (List.length (Buffer.lines buffer) - 1) (n_ - 1)

                        cursor =
                            { line = n, column = 0 }

                        window =
                            Window.scrollToIncludeCursor cursor state.window
                    in
                    ( { state | cursor = cursor, window = window, selection = Nothing }, buffer, Cmd.none ) |> recordHistory state buffer
```


The End
"""


long =
    List.repeat 30 jabberwocky |> String.join "\n----\n\n"


jabberwocky =
    """Jabberwocky

By Lewis Carroll

’Twas brillig, and the slithy toves
     Did gyre and gimble in the wabe:
All mimsy were the borogoves,
     And the mome raths outgrabe.

“Beware the Jabberwock, my son!
     The jaws that bite, the claws that catch!
Beware the Jubjub bird, and shun
     The frumious Bandersnatch!”

He took his vorpal sword in hand;
     Long time the manxome foe he sought—
So rested he by the Tumtum tree
     And stood awhile in thought.

And, as in uffish thought he stood,
     The Jabberwock, with eyes of flame,
Came whiffling through the tulgey wood,
     And burbled as it came!

One, two! One, two! And through and through
     The vorpal blade went snicker-snack!
He left it dead, and with its head
     He went galumphing back.

“And hast thou slain the Jabberwock?
     Come to my arms, my beamish boy!
O frabjous day! Callooh! Callay!”
     He chortled in his joy.

’Twas brillig, and the slithy toves
     Did gyre and gimble in the wabe:
All mimsy were the borogoves,
     And the mome raths outgrabe.

PS. Here is the buried treasure.

NOTES

1. The above text about "treasure" is **fake**.
We were just testing to see if we could send
the editor requests like "find the word 'treasure,'
scroll down to it, and highlight it."

2. Now that this is working, we have a bit of
code cleanup to do. And more work on some
cursor and highight flakines, e.g., highlighting
should be preserved when scrolling.


"""
