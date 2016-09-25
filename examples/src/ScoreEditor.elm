module ScoreEditor exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onClick, onInput)
import Html.App as Html
import String exposing (foldr, cons)
import Maybe exposing (withDefault)
import Maybe.Extra exposing (isJust)
import Json.Decode as Json exposing (succeed)
import Abc exposing (..)
import Abc.ParseTree exposing (AbcTune)
import Music.Notation exposing (getTitle)
import VexTab exposing (..)
import VexTab.Config exposing (Config)
import VexScore.Translate exposing (translate)
import VexScore.Canonical exposing (toScoreText)
import FileIO.Ports exposing (..)
import Debug exposing (log)


main =
    Html.program
        { init = init, update = update, view = view, subscriptions = subscriptions }


type Msg
    = AbcText String
      -- get the ABC text
    | TranslateToVex
      -- translate it to Vex
    | VexTabMsg VexTab.Msg
      -- delegate to VexTab
    | RequestFileUpload
      -- request an ABC upload
    | RequestFileDownload
      -- request an ABC download
    | FileLoaded (Maybe Filespec)
      -- returned loaded ABC
    | NoOp


type alias Model =
    { abc : String
    , tuneResult : Result ParseError AbcTune
    , fileName : Maybe String
    , vextab : VexTab.Model
    , error : Maybe String
    }


defaultConfig : Config
defaultConfig =
    { canvasDivId = "#vextab"
    , canvasX = 10
    , canvasY = 10
    , canvasWidth = 1200
    , scale = 0.8
    }


emptyTune : AbcTune
emptyTune =
    ( [], [] )


{-| initialise the model and delegate the initial command to that of the vextab module
-}
init : ( Model, Cmd Msg )
init =
    let
        ( vextabModel, vextabCmd ) =
            VexTab.init defaultConfig
    in
        { abc = ""
        , tuneResult = Ok emptyTune
        , fileName = Nothing
        , vextab = vextabModel
        , error = Nothing
        }
            ! [ Cmd.map VexTabMsg vextabCmd ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AbcText s ->
            let
                newModel =
                    { model | abc = s, error = Nothing }
            in
                update TranslateToVex newModel

        NoOp ->
            ( model, Cmd.none )

        TranslateToVex ->
            let
                parseResult =
                    parse (terminateLine model.abc)

                translationResult =
                    case parseResult of
                        Ok tune ->
                            translate tune

                        Err e ->
                            Err ("parse error: " ++ (parseError e))
            in
                case translationResult of
                    Ok vexScore ->
                        let
                            ( newVextab, cmd ) =
                                VexTab.update (VexTab.RequestRenderScore (toScoreText vexScore)) model.vextab
                        in
                            { model | vextab = newVextab, tuneResult = parseResult } ! [ Cmd.map VexTabMsg cmd ]

                    Err e ->
                        { model | error = Just e, tuneResult = parseResult } ! [ Cmd.none ]

        RequestFileUpload ->
            ( model, requestLoadFile () )

        RequestFileDownload ->
            let
                fileName =
                    getFileName model

                filespec =
                    Filespec model.abc fileName
            in
                ( model, requestSaveFile filespec )

        FileLoaded maybef ->
            case maybef of
                Just f ->
                    let
                        newModel =
                            { model
                                | abc = f.contents
                                , fileName = Just f.name
                            }

                        _ =
                            log "file contents" f.contents
                    in
                        let
                            _ =
                                log "file not loaded" ()
                        in
                            update TranslateToVex newModel

                Nothing ->
                    ( model, Cmd.none )

        VexTabMsg vextabMsg ->
            let
                ( newVextab, cmd ) =
                    VexTab.update vextabMsg model.vextab
            in
                { model | vextab = newVextab } ! [ Cmd.map VexTabMsg cmd ]


terminateLine : String -> String
terminateLine s =
    s ++ "\x0D\n"



{- get the file name of the tune.  This is either (in order)
   the name of the file that was originally loaded
   the tune title (if present) with .abc appended
   untitled.abc
-}


getFileName : Model -> String
getFileName m =
    case m.fileName of
        Just name ->
            name

        _ ->
            case
                m.tuneResult
            of
                Ok tune ->
                    (getTitle tune
                        |> withDefault "untitled"
                    )
                        ++ ".abc"

                _ ->
                    "untitled.abc"



-- overall subscriptions
-- subscription from the FileIO port


fileLoadedSub : Sub Msg
fileLoadedSub =
    fileLoaded FileLoaded


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ -- subscription from the vex tab module
          Sub.map VexTabMsg (VexTab.subscriptions model.vextab)
        , fileLoadedSub
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


view : Model -> Html Msg
view model =
    div []
        [ div [ leftPaneStyle ]
            [ span [ leftPanelLabelStyle ] [ text "Load an ABC file:" ]
            , input
                [ type' "file"
                , id "fileinput"
                  -- FileIO port requires this exact id to be set
                , accept ".abc, .txt"
                  --, onClick RequestFileUpload
                , on "change" (Json.succeed RequestFileUpload)
                , inputStyle
                ]
                []
            , span [ leftPanelLabelStyle ]
                [ text "Save ABC to file:"
                , button (buttonAttributes True RequestFileDownload)
                    [ text "save" ]
                ]
            ]
        , div [ rightPaneStyle ]
            [ textarea
                [ placeholder "ABC"
                , value model.abc
                , onInput AbcText
                , taStyle
                , cols 78
                , rows 16
                , autocomplete False
                , spellcheck False
                , autofocus True
                ]
                []
            , div []
                [ viewMaybe model.vextab.error ]
              {-
                 , div []
                     [ viewMaybe model.vextab.text ]
              -}
            , div []
                [ viewMaybe model.error ]
            , div []
                [ canvas
                    [ id "vextab"
                    , hidden (isJust model.error || isJust model.vextab.error)
                    ]
                    []
                ]
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



{- style an input -}


inputStyle : Attribute Msg
inputStyle =
    style
        [ ( "padding", "10px 0" )
        , ( "font-size", "1em" )
        , ( "margin-left", "40px" )
        ]


leftPanelLabelStyle : Attribute msg
leftPanelLabelStyle =
    style
        [ ( "margin-left", "40px" )
        , ( "margin-top", "40px" )
        , ( "font-size", "1.2em" )
        ]


leftPaneStyle : Attribute msg
leftPaneStyle =
    style
        [ ( "float", "left" )
        , ( "width", "350px" )
        ]


rightPaneStyle : Attribute msg
rightPaneStyle =
    style
        [ ( "float", "left" )
        ]



{- gather together all the button attributes

   In this version of the editor, buttons are enabled all the time.  They stop the tune
   when they are selected if the tune happens to be playing
-}


buttonAttributes : Bool -> Msg -> List (Attribute Msg)
buttonAttributes isEnabled msg =
    [ class "hoverable"
    , bStyle isEnabled
    , onClick msg
    , disabled (not isEnabled)
    ]



{- style a button
   Note: all button styling is deferred to the external css (which implements hover)
         except for when the button is greyed out when it is disabled
-}


bStyle : Bool -> Attribute msg
bStyle enabled =
    let
        colour =
            if enabled then
                []
            else
                [ ( "background-color", "lightgray" )
                , ( "color", "darkgrey" )
                ]

        textSize =
            [ ( "font-size", "1em" ) ]
    in
        style (colour ++ textSize)
