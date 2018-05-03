module Main exposing (..)

import Html exposing (Html, text, div, input, label)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (class, value, type_, checked, placeholder)
import Trivium


---- MODEL ----


type SelectionType
    = Include
    | Exclude


type Header
    = Name
    | Title
    | Location


type alias Entry =
    { name : String
    , title : String
    , location : String
    , checked : SelectionType
    , selectionType : SelectionType
    }


type alias Model =
    { query : String
    , selection : List Entry
    , table : Trivium.State Entry
    }


init : ( Model, Cmd Msg )
init =
    ( (Model "")
        []
        (Trivium.initWith
            initEntries
            (\entry -> entry.selectionType == Include && entry.checked == Include)
            (\entry -> entry.selectionType == Include)
            (\entry -> entry.checked == Include)
            (\entry bool ->
                { entry
                    | checked =
                        if bool then
                            if entry.checked == Include then
                                Exclude
                            else
                                Include
                        else
                            Exclude
                }
            )
            (\entry query ->
                { entry
                    | selectionType =
                        if String.contains (String.toLower query) (String.toLower entry.name) then
                            Include
                        else
                            Exclude
                }
            )
        )
    , Cmd.none
    )


initEntries : List Entry
initEntries =
    [ { checked = Include, selectionType = Include, name = "Matt", title = "Lead Developer", location = "Austin" }
    , { checked = Include, selectionType = Include, name = "Justin", title = "Founder", location = "Texas" }
    , { checked = Include, selectionType = Include, name = "Bailey", title = "Developer", location = "Texas" }
    , { checked = Include, selectionType = Include, name = "Quincy", title = "Business", location = "Texas" }
    , { checked = Include, selectionType = Include, name = "Sarah", title = "Boss", location = "New Jersey" }
    , { checked = Include, selectionType = Include, name = "Carolyn", title = "Assistant Boss", location = "Austin" }
    , { checked = Include, selectionType = Include, name = "Luna", title = "Assistant to the Boss", location = "Moon" }
    ]



---- UPDATE ----


type Msg
    = NoOp
    | UpdateQuery String
    | TriviumMsg (Trivium.Msg Entry)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateQuery str ->
            ( { model | query = str }, Cmd.none )

        TriviumMsg triviumMsg ->
            let
                ( state, msg ) =
                    Trivium.update triviumMsg model.table
            in
                ( { model | table = state }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div [ class "body__container" ]
        [ Html.map (\a -> TriviumMsg a) <| Trivium.viewFilter selection model.table
        , div [ class "input__container" ]
            [ input
                [ type_ "text"
                , class "search-input"
                , placeholder "Type text to filter..."
                , onInput UpdateQuery
                , value model.query
                ]
                []
            ]
        , div [ class "table__container" ]
            [ div [ class "table" ] []
            ]
        , Html.map (\a -> TriviumMsg a) <| Trivium.view model.table
        ]


selection : Entry -> String
selection entry =
    entry.name



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
