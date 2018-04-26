module Main exposing (..)

import Html exposing (Html, text, div, input, label)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (class, value, type_)


---- MODEL ----


type SelectionType
    = Include
    | Exclude


type alias State a =
    { id : String
    , query : String
    , entries : List Entry
    , selected : Maybe (List a)
    , open : Bool
    , dropdownSelection : List String
    }


type alias Entry =
    { name : String
    , title : String
    , location : String
    , selectionType : SelectionType
    }


init : ( State a, Cmd Msg )
init =
    ( State "" "" initEntries Nothing False [], Cmd.none )


initEntries : List Entry
initEntries =
    [ { name = "Matt", title = "Lead Developer", location = "Austin", selectionType = Include }
    , { name = "Justin", title = "Founder", location = "Texas", selectionType = Include }
    , { name = "Bailey", title = "Developer", location = "Texas", selectionType = Include }
    , { name = "Quincy", title = "Business", location = "Texas", selectionType = Include }
    , { name = "Sarah", title = "Boss", location = "New Jersey", selectionType = Include }
    , { name = "Carolyn", title = "Assistant Boss", location = "Austin", selectionType = Include }
    , { name = "Luna", title = "Assistant to the Boss", location = "Moon", selectionType = Include }
    ]



---- UPDATE ----


type Msg
    = NoOp
    | UpdateQuery String
    | Dropdown String


update : Msg -> State a -> ( State a, Cmd Msg )
update msg state =
    case msg of
        UpdateQuery query ->
            ( { state
                | query = query
                , entries = filterEntries query state.entries
              }
            , Cmd.none
            )

        Dropdown header ->
            ( { state
                | open = not state.open
                , dropdownSelection = "All" :: List.map (headerToEntryField header) state.entries
              }
            , Cmd.none
            )

        _ ->
            ( state, Cmd.none )


headerToEntryField : String -> Entry -> String
headerToEntryField header entry =
    case header of
        "Name" ->
            entry.name

        "Title" ->
            entry.title

        "Location" ->
            entry.location

        _ ->
            ""


setDropdown : State a -> ( State a, Cmd Msg )
setDropdown state =
    ( { state
        | open = not state.open
        , dropdownSelection = List.map (\entry -> entry.name) state.entries
      }
    , Cmd.none
    )


filterEntries : String -> List Entry -> List Entry
filterEntries query entries =
    List.map
        (\entry ->
            let
                queryInName =
                    String.contains (String.toLower query) (String.toLower entry.name)

                queryInTitle =
                    String.contains (String.toLower query) (String.toLower entry.title)

                queryInLocation =
                    String.contains (String.toLower query) (String.toLower entry.location)
            in
                if queryInName || queryInTitle || queryInLocation then
                    { entry | selectionType = Include }
                else
                    { entry | selectionType = Exclude }
        )
        entries



---- VIEW ----


view : State a -> Html Msg
view state =
    div [ class "body__container" ]
        [ div [ class "input__container" ]
            [ input [ type_ "text", class "search-input", onInput UpdateQuery, value state.query ] []
            , div [ class "query-text" ] [ text state.query ]
            ]
        , div [ class "table__container" ] <| showEntries state
        , div [] [ showDropdown state ]
        ]


showEntries : State a -> List (Html Msg)
showEntries state =
    let
        filteredEntries =
            List.filter (\entry -> entry.selectionType == Include) state.entries
    in
        case filteredEntries of
            [] ->
                [ div [ class "table" ] [ text "No Matches" ] ]

            _ ->
                [ div [ class "table" ]
                    [ div [ class "table__row headers" ] <| tableHeaders
                    , div [ class "table__body" ] <| tableBody filteredEntries
                    ]
                ]


tableHeaders : List (Html Msg)
tableHeaders =
    let
        headers =
            [ "Name", "Title", "Location" ]
    in
        List.map
            (\header ->
                div [ class "table__entry-block flex" ]
                    [ div [ class "text" ] [ text header ]
                    , div [ class "drop-down", onClick (Dropdown header) ] [ text "^" ]
                    ]
            )
            headers


tableBody : List Entry -> List (Html Msg)
tableBody entries =
    List.map
        (\entry ->
            div [ class "table__row" ]
                [ div [ class "table__entry-block" ] [ text entry.name ]
                , div [ class "table__entry-block" ] [ text entry.title ]
                , div [ class "table__entry-block" ] [ text entry.location ]
                ]
        )
        entries


showDropdown : State a -> Html Msg
showDropdown state =
    if state.open then
        div [ class "dropdown-list" ] <|
            List.map
                (\item ->
                    label []
                        [ input [ type_ "checkbox" ] []
                        , text item
                        ]
                )
                state.dropdownSelection
    else
        text ""



---- PROGRAM ----


main : Program Never (State a) Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
