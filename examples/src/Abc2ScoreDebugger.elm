module Abc2ScoreDebugger exposing (..)

import Html exposing (Html, Attribute, text, div, input, button, textarea, canvas, p)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onClick, onInput)
import Html.App as Html
import String exposing (foldr, cons)
import Maybe.Extra exposing (isJust)
import VexTab exposing (..)
import VexTab.Config exposing (Config)
import VexScore.Translate exposing (translateText)
import VexScore.Canonical exposing (toScoreText)
import Debug exposing (log)


main =
    Html.program
        { init = init, update = update, view = view, subscriptions = subscriptions }


type Msg
    = AbcText String
    | TranslateToVex
    | VexTabMsg VexTab.Msg
    | NoOp


type VexTranslationState
    = Untranslated
    | Translated (Maybe String)


type alias Model =
    { abc : String
    , vexTranslationState : VexTranslationState
    , vextab : VexTab.Model
    }


defaultConfig : Config
defaultConfig =
    { canvasDivId = "#vextab"
    , canvasX = 10
    , canvasY = 10
    , canvasWidth = 1200
    , scale = 0.8
    }


{-| initialise the model and delegate the initial command to that of the vextab module
-}
init : ( Model, Cmd Msg )
init =
    let
        ( vextabModel, vextabCmd ) =
            VexTab.init defaultConfig
    in
        { abc = ""
        , vexTranslationState = Untranslated
        , vextab = vextabModel
        }
            ! [ Cmd.map VexTabMsg vextabCmd ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AbcText s ->
            ( { model
                | abc = s
                , vextab = VexTab.Model Nothing Nothing
                , vexTranslationState = Untranslated
              }
            , Cmd.none
            )

        NoOp ->
            ( model, Cmd.none )

        TranslateToVex ->
            let
                translationResult =
                    translateText (terminateLine model.abc)
            in
                case translationResult of
                    Ok vexScore ->
                        let
                            ( newVextab, cmd ) =
                                VexTab.update (VexTab.RequestRenderScore (toScoreText vexScore)) model.vextab
                        in
                            { model
                                | vextab = newVextab
                                , vexTranslationState = Translated Nothing
                            }
                                ! [ Cmd.map VexTabMsg cmd ]

                    Err e ->
                        { model | vexTranslationState = Translated (Just e) } ! [ Cmd.none ]

        VexTabMsg vextabMsg ->
            let
                ( newVextab, cmd ) =
                    VexTab.update vextabMsg model.vextab
            in
                { model | vextab = newVextab } ! [ Cmd.map VexTabMsg cmd ]


terminateLine : String -> String
terminateLine s =
    s ++ "\x0D\n"



-- overall subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ -- subscription from the vex tab module
          Sub.map VexTabMsg (VexTab.subscriptions model.vextab)
        ]



-- VIEW


formatLines : String -> Html Msg
formatLines s =
    let
        components =
            String.split "\x0D\n" s

        f s =
            p [] [ text s ]
    in
        div []
            (List.map f components)


viewMaybe : Maybe String -> Html Msg
viewMaybe mpe =
    case mpe of
        Just e ->
            formatLines e

        _ ->
            text ""


viewTranslationState : VexTranslationState -> Html Msg
viewTranslationState vte =
    case vte of
        Untranslated ->
            text ""

        Translated maybe ->
            viewMaybe maybe



{- hide the score if it's untranslated into Vex or if it's a vexNote
   translation error or a vex error in producing the score
-}


hideScore : Model -> Bool
hideScore model =
    case model.vexTranslationState of
        Translated Nothing ->
            isJust model.vextab.error

        _ ->
            True


view : Model -> Html Msg
view model =
    div []
        [ textarea
            [ placeholder "ABC"
            , value model.abc
            , onInput AbcText
            , taStyle
            , cols 70
            , rows 16
            , autocomplete False
            , spellcheck False
            , autofocus True
            ]
            []
        , button
            [ -- onClick (VexTabMsg (VexTab.RequestRenderScore model.text))
              onClick TranslateToVex
            , id "elm-render-score"
            , btnStyle
            ]
            [ text "render score" ]
        , div []
            [ viewMaybe model.vextab.error ]
        , div []
            [ viewMaybe model.vextab.text ]
        , div []
            [ viewTranslationState model.vexTranslationState ]
        , div []
            [ canvas
                [ id "vextab"
                  -- , hidden (isTranslationError model.vexTranslationState || isJust model.vextab.error)
                , hidden (hideScore model)
                ]
                []
            ]
        ]



{- style a textarea -}


taStyle : Attribute Msg
taStyle =
    style
        [ ( "padding", "10px 0" )
        , ( "font-size", "1.5em" )
        , ( "text-align", "left" )
        , ( "align", "center" )
        , ( "display", "block" )
        , ( "margin-left", "auto" )
        , ( "margin-right", "auto" )
        , ( "background-color", "#f3f6c6" )
        , ( "font-family", "monospace" )
        ]


btnStyle : Attribute msg
btnStyle =
    style
        [ ( "font-size", "1em" )
        , ( "text-align", "center" )
        ]
