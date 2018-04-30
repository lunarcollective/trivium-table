module Main exposing (..)

import Html exposing (Html, text, div, input, label)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (class, value, type_, checked)
import Set exposing (Set)


---- MODEL ----


type SelectionType
    = Include
    | Exclude


type Header
    = Name
    | Title
    | Location


type Dir
    = Asc
    | Desc


type SortType
    = SortType Dir Header


type alias State a =
    { id : String
    , query : String
    , entries : List Entry
    , selected : Maybe (List a)
    , open : Bool
    , dropdownSelection : Set String
    , checkedEntries : Set String
    , sortType : SortType
    }


type alias Entry =
    { name : String
    , title : String
    , location : String
    , selectionType : SelectionType
    , checked : SelectionType
    }


init : ( State a, Cmd Msg )
init =
    ( State "" "" initEntries Nothing False (Set.fromList []) (Set.fromList []) (SortType Asc Name), Cmd.none )


initEntries : List Entry
initEntries =
    [ { name = "Matt", title = "Lead Developer", location = "Austin", selectionType = Include, checked = Include }
    , { name = "Justin", title = "Founder", location = "Texas", selectionType = Include, checked = Include }
    , { name = "Bailey", title = "Developer", location = "Texas", selectionType = Include, checked = Include }
    , { name = "Quincy", title = "Business", location = "Texas", selectionType = Include, checked = Include }
    , { name = "Sarah", title = "Boss", location = "New Jersey", selectionType = Include, checked = Include }
    , { name = "Carolyn", title = "Assistant Boss", location = "Austin", selectionType = Include, checked = Include }
    , { name = "Luna", title = "Assistant to the Boss", location = "Moon", selectionType = Include, checked = Include }
    ]



---- UPDATE ----


type Msg
    = NoOp
    | UpdateQuery String
    | Dropdown String
    | FilterChecked String
    | AlphaSort String


toName : { a | name : String } -> String
toName obj =
    obj.name


update : Msg -> State a -> ( State a, Cmd Msg )
update msg state =
    case msg of
        UpdateQuery query ->
            ( { state
                | query = query
                , entries = List.map (filterEntries query) state.entries
              }
            , Cmd.none
            )

        Dropdown header ->
            ( { state
                | open = not state.open
                , dropdownSelection = Set.fromList <| "All" :: List.map (headerToEntryField header state) state.entries
              }
            , Cmd.none
            )

        FilterChecked string ->
            let
                newSet =
                    if Set.member string state.checkedEntries then
                        Set.remove string state.checkedEntries
                    else
                        Set.insert string state.checkedEntries
            in
                ( { state
                    | checkedEntries =
                        newSet
                    , entries = List.map (filterCheckedEntries newSet) state.entries
                  }
                , Cmd.none
                )

        AlphaSort header ->
            let
                sortType =
                    setSortType header

                direction =
                    setDirection sortType state
            in
                ( { state
                    | sortType = SortType direction sortType
                  }
                , Cmd.none
                )

        _ ->
            ( state, Cmd.none )


setDirection : Header -> State a -> Dir
setDirection sortType state =
    case state.sortType of
        SortType dir currentHeader ->
            if currentHeader == sortType then
                case dir of
                    Asc ->
                        Desc

                    Desc ->
                        Asc
            else
                Asc


setSortType : String -> Header
setSortType header =
    case header of
        "Name" ->
            Name

        "Title" ->
            Title

        "Location" ->
            Location

        _ ->
            Name


filterCheckedEntries : Set String -> Entry -> Entry
filterCheckedEntries stringSet entry =
    let
        nameInSet =
            Set.member entry.name stringSet

        titleInSet =
            Set.member entry.title stringSet

        locationInSet =
            Set.member entry.location stringSet
    in
        if nameInSet || titleInSet || locationInSet then
            { entry | checked = Exclude }
        else
            { entry | checked = Include }


filterEntries : String -> Entry -> Entry
filterEntries query entry =
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


headerToEntryField : String -> State a -> Entry -> String
headerToEntryField header state entry =
    case header of
        "Name" ->
            if String.contains (String.toLower state.query) (String.toLower entry.name) then
                entry.name
            else
                ""

        "Title" ->
            entry.title

        "Location" ->
            entry.location

        _ ->
            ""



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
            List.filter (\entry -> entry.selectionType == Include && entry.checked == Include) state.entries |> sortedEntries state
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
                    [ div [ class "text sortable", onClick (AlphaSort header) ] [ text header ]
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
                        [ input [ type_ "checkbox", checked <| not (Set.member item state.checkedEntries), onClick (FilterChecked item) ] []
                        , text item
                        ]
                )
                (Set.toList state.dropdownSelection)
    else
        text ""


sortedEntries : State a -> List Entry -> List Entry
sortedEntries state entries =
    case state.sortType of
        SortType dir header ->
            let
                sorted =
                    case header of
                        Name ->
                            List.sortBy .name entries

                        Title ->
                            List.sortBy .title entries

                        Location ->
                            List.sortBy .location entries
            in
                case dir of
                    Asc ->
                        sorted

                    Desc ->
                        List.reverse sorted



---- PROGRAM ----


main : Program Never (State a) Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
