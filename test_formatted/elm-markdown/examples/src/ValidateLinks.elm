module ValidateLinks exposing (main)

import Browser
import Html exposing (Attribute, Html, div, text)
import Html.Attributes as Attr
import Html.Events
import Markdown.Block exposing (Block, Inline)
import Markdown.Parser as Markdown
import Markdown.Renderer


view : String -> Html Msg
view markdownInput =
    Html.div [ Attr.style "padding" "20px" ]
        [ markdownInputView markdownInput
        , case
            markdownInput
                |> Markdown.parse
                |> Result.mapError deadEndsToString
                |> Result.andThen (\ast -> Markdown.Renderer.render Markdown.Renderer.defaultHtmlRenderer ast)
          of
            Ok rendered ->
                div [] rendered

            Err errors ->
                text errors
        ]


validateMap : (Block -> Result error value) -> List Block -> Result (List error) (List value)
validateMap mapFn blocks =
    --Ok blocks
    Ok []


validateMapInlines : (Inline -> Result error Block) -> List Block -> Result (List error) (List Block)
validateMapInlines mapFn blocks =
    Ok blocks


markdownInputView : String -> Html Msg
markdownInputView markdownInput =
    Html.textarea
        [ Attr.value markdownInput
        , Html.Events.onInput OnMarkdownInput
        , Attr.style "width" "100%"
        , Attr.style "height" "500px"
        , Attr.style "font-size" "18px"
        ]
        []


deadEndsToString deadEnds =
    deadEnds
        |> List.map Markdown.deadEndToString
        |> String.join "\n"


markdownBody =
    """
# 1 Esse intrata referre inter adspeximus aequora soror

## 2 Ebur iamque mecum

2 Lorem markdownum [vitae](http://minantia.io/herbis-caelumque.aspx) crines
carminibus exponit pugnax dilectaque Sparte te est. Nullos imperium! Ait qui
corpora perstat Gryneus fidem iunctura. Hic sperne inquit iuventus timidasque
iuvenis stirpe barbarus sorori? Fatebor non in iaculatur, concuteret auget
corpore accepere vectus pacisci quoque renascitur essem: frugum labentibus
Naxoque festum despectat.

## 3 Saepe nec tergo Iole te solent pharetras

3 Iamque insula, ore longe dixerat libratum neque terrarum resedit de iuranda cum
muneris *tamen*, suas populique te. Alumno invidiae cecinit exarsit modo vidit
ingentia suum, et pluribus sensu *Danais* adigitque acervo gravis visae,
capillos!

    3avar post = client_script_agp(bar_address_golden(flash, digital_halftone_unix
            + 3), supercomputer_bridge);
    type_ctr_drag(waveform, -2, core_rom);
    if (frameworkFile * wiki_ddr >= architecture_core(station_mebibyte, 5,
            dual_cd)) {
        tft.logMirror(infotainmentPram + spoofingArchitectureServer,
                sliRiscParse + mmsClean);
    }

## 4 Vetustas caede

4 Grata raucaque dixit delenda terris. Actorum circumdata fronde fuerat, accepisse
certe, haurit manu. [Ventura Achille](http://www.devolenti.net/canoro.php)
admovit, non ut tempto violas est ego pater; fit probavit iaculi Ophiusiaque
inque. Conlectae est premebat subsunt. Dum ad adusque sol sub vini, quod: per
sanguine, recludit posuisti: Trinacris Sibyllae.

## 5 Tamquam novus

5 Nec munera pia sequuntur consedit est vultus, **enim laeva**, hortaturque
sulphura fraterna somni [circumstantes](http://illis-vacant.io/susmater.php)?
Futurae habet visa cogit natus coeperat lacertos luxuriem, coloribus quaecumque
unus membra? Et molirique saevior terrae concubiturus pars. Aequor convivia ergo
nec salutem, absentem veris exspirat, traxit deiectoque dedignata evolat pressit
me promissa amor ardor.

5b Iuvenem fluunt populusque iterum arcet donavi testatos tellus semperque
debueram. Edidit [illo votis](http://sum.net/cumfelixque) Melicerta *vivus*,
mare, **praefoderat iubasque**? Dabat **temerarius boves orbe** populi!

## 6 Sic colitur tecum exsultantemque fessis vidit rescindere

6 Polydoreo Iovis mentis fratre posse, claudit placabilis nisi radiante premunt,
cum committitur, inquit bovem caput, vocem! De sensit vestigia super. Effugit
nux tamen nota pererrat nec semel erat: quater e solvi non nec **inmitem
tristi**.

6b Talia litore glomerataque quantum lentaque **restat**, nec lapsa Threiciis
subiere tamen exercere et tuis. Est fine cum supposito iamque, ex templa illa
cursus venerit tenebat et? Quemque mihi, dare erudit Lyncus, ab dicebar iterum
exanimi sermone; esse Iunonem paelicis mundi velit.

6c *Tydiden dubitabile neque* conscendere ardor verboque sic refert Auroram
sequantur praemia doleam pectusque fumantia hospes, cum silvaque caputque. Domat
et annis corpus est aperire amoris. Concha non quae columbas, quae tenuem,
pervia, euntis?
"""


type Msg
    = OnMarkdownInput String


type alias Flags =
    ()


type alias Model =
    String


main : Platform.Program Flags Model Msg
main =
    Browser.document
        { init = \flags -> ( markdownBody, Cmd.none )
        , view = \model -> { body = [ view model ], title = "Markdown Example" }
        , update = update
        , subscriptions = \model -> Sub.none
        }


update msg model =
    case msg of
        OnMarkdownInput newMarkdown ->
            ( newMarkdown, Cmd.none )
