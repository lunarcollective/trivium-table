module Main exposing (..)

import Html exposing (Html, text, div, h1, img, input)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (class, value, type_)


---- MODEL ----


type SelectionType
    = Include
    | Exclude


type alias State a =
    { id : String
    , query : String
    , entries : Maybe (List Entry)
    , selected : Maybe (List a)
    , open : Bool
    , dropdownSelection : Maybe (List a)
    }


type alias Entry =
    { name : String
    , title : String
    , location : String
    , selectionType : SelectionType
    }


init : ( State a, Cmd Msg )
init =
    ( State "" "" (Just initEntries) Nothing False Nothing, Cmd.none )


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
    | ShowDropdown


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

        ShowDropdown ->
            ( { state | open = not state.open }, Cmd.none )

        -- case header of
        --   "Name" ->
        --
        --   "Title" ->
        --   "Location" ->
        _ ->
            ( state, Cmd.none )


filterEntries : String -> Maybe (List Entry) -> Maybe (List Entry)
filterEntries query entries =
    Maybe.map
        (\list ->
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
                list
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
        , if state.open then
            div [] [ text "OH MY GOD" ]
          else
            text ""
        ]


showEntries : State a -> List (Html Msg)
showEntries state =
    case state.entries of
        Just entries ->
            let
                filteredEntries =
                    List.filter (\entry -> entry.selectionType == Include) entries
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

        Nothing ->
            [ div [ class "table" ] [ text "No List" ] ]


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
                    , div [ class "drop-down", onClick ShowDropdown ] [ text "^" ]
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



---- PROGRAM ----


main : Program Never (State a) Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
