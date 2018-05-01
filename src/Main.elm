module Main exposing (..)

import Html exposing (Html, text, div, input, label)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (class, value, type_, checked, placeholder)
import Set exposing (Set)


---- MODEL ----


type SelectionType
    = Include
    | Exclude


type Header
    = Name
    | Title
    | Location


type Dropdown
    = Open
    | Closed


type Dir
    = Asc
    | Desc


type SortType
    = SortType Dir Header


type alias State =
    { id : String
    , query : String
    , entries : List Entry
    , selected : List Entry
    , open : ( Dropdown, Header )
    , dropdownSelection : List ( String, Header )
    , uncheckedEntries : List ( String, Header )
    , sortType : SortType
    }


type alias Entry =
    { name : String
    , title : String
    , location : String
    , selectionType : SelectionType
    , checked : SelectionType
    }


init : ( State, Cmd Msg )
init =
    ( State
        ""
        ""
        initEntries
        []
        ( Closed, Name )
        ([])
        ([])
        (SortType Asc Name)
    , Cmd.none
    )


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
    | FilterChecked ( String, Header )
    | AlphaSort String
    | Select Entry
    | Deselect Entry


toName : { a | name : String } -> String
toName obj =
    obj.name


update : Msg -> State -> ( State, Cmd Msg )
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
            let
                sortType =
                    setSortType header
            in
                ( { state
                    | open = toggleFilter header state
                    , dropdownSelection = ( "All", sortType ) :: List.map (headerToEntryField sortType state) state.entries
                  }
                , Cmd.none
                )

        FilterChecked ( item, header ) ->
            let
                filterSet =
                    case ( item, header ) of
                        ( "All", header ) ->
                            if List.member ( "All", header ) state.uncheckedEntries then
                                List.filter
                                    (\( _, hdr ) -> hdr /= header)
                                    state.uncheckedEntries
                            else
                                ( "All", header ) :: state.uncheckedEntries ++ List.map (\entry -> ( getEntryString header entry, header )) state.entries

                        ( item, header ) ->
                            if List.member ( item, header ) state.uncheckedEntries then
                                List.filter (\( itm, hdr ) -> ( itm, hdr ) /= ( item, header )) state.uncheckedEntries
                            else
                                ( item, header ) :: state.uncheckedEntries
            in
                ( { state
                    | uncheckedEntries = filterSet
                    , entries = List.map (filterUncheckedEntries ( item, header ) filterSet) state.entries
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

        Select entry ->
            ( { state | selected = insert entry state.selected }, Cmd.none )

        Deselect entry ->
            ( { state | selected = remove entry state.selected }, Cmd.none )

        _ ->
            ( state, Cmd.none )


insert : Entry -> List Entry -> List Entry
insert entry selected =
    entry :: selected


remove : Entry -> List Entry -> List Entry
remove entry selected =
    List.filter (\e -> e /= entry) selected


toggleFilter : String -> State -> ( Dropdown, Header )
toggleFilter header state =
    let
        headerAction =
            setSortType header
    in
        case state.open of
            ( Open, hdr ) ->
                if hdr == headerAction then
                    ( Closed, headerAction )
                else
                    ( Open, headerAction )

            ( Closed, _ ) ->
                ( Open, headerAction )


setDirection : Header -> State -> Dir
setDirection sortType state =
    case state.sortType of
        SortType dir currentHeader ->
            if currentHeader == sortType then
                flipDir dir
            else
                Asc


flipDir : Dir -> Dir
flipDir dir =
    case dir of
        Asc ->
            Desc

        Desc ->
            Asc


getEntryString : Header -> Entry -> String
getEntryString header entry =
    case header of
        Name ->
            entry.name

        Title ->
            entry.title

        Location ->
            entry.location


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


filterUncheckedEntries : ( String, Header ) -> List ( String, Header ) -> Entry -> Entry
filterUncheckedEntries ( item, header ) filterSet entry =
    let
        nameInSet =
            List.member ( entry.name, Name ) filterSet

        titleInSet =
            List.member ( entry.title, Title ) filterSet

        locationInSet =
            List.member ( entry.location, Location ) filterSet

        headers =
            [ Name, Title, Location ]
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


headerToEntryField : Header -> State -> Entry -> ( String, Header )
headerToEntryField sortType state entry =
    case sortType of
        Name ->
            ( entry.name, sortType )

        Title ->
            ( entry.title, sortType )

        Location ->
            ( entry.location, sortType )



---- VIEW ----


view : State -> Html Msg
view state =
    div [ class "body__container" ]
        [ div [ class "input__container" ]
            [ input [ type_ "text", class "search-input", placeholder "Type text to filter...", onInput UpdateQuery, value state.query ] [] ]
        , div [ class "table__container" ]
            [ div [ class "table" ]
                [ div [ class "table__row headers" ] <| tableHeaders state
                , div [] <| showEntries state
                ]
            ]
        ]


showEntries : State -> List (Html Msg)
showEntries state =
    let
        filteredEntries =
            List.filter (\entry -> entry.selectionType == Include && entry.checked == Include) state.entries |> sortedEntries state
    in
        case filteredEntries of
            [] ->
                [ div [] [ text "No Matches" ] ]

            _ ->
                [ div [] <| tableBody filteredEntries state.selected ]


tableHeaders : State -> List (Html Msg)
tableHeaders state =
    let
        headers =
            [ ( Name, "Name" ), ( Title, "Title" ), ( Location, "Location" ) ]
    in
        div [ class "table__entry--block flex" ] [ text "" ]
            :: List.map
                (\( action, string ) ->
                    div [ class "table__entry--block flex" ]
                        [ div [ class "table__entry--text sortable", onClick (AlphaSort string) ] [ text string ]
                        , div [ class "drop-down-and-button" ]
                            [ div [ class "drop-down", onClick (Dropdown string) ]
                                [ text "+" ]
                            , div [] [ showDropdown action state ]
                            ]
                        ]
                )
                headers


tableBody : List Entry -> List Entry -> List (Html Msg)
tableBody entries selected =
    List.map
        (\entry ->
            div [ class "table__row entry" ]
                [ checkbox selected entry
                , div [ class "table__entry--block" ] [ text entry.name ]
                , div [ class "table__entry--block" ] [ text entry.title ]
                , div [ class "table__entry--block" ] [ text entry.location ]
                ]
        )
        entries


checkbox : List Entry -> Entry -> Html Msg
checkbox selected entry =
    let
        isChecked =
            List.member entry selected

        message =
            if isChecked then
                Deselect entry
            else
                Select entry
    in
        input [ type_ "checkbox", checked isChecked, onClick message ] []


showDropdown : Header -> State -> Html Msg
showDropdown header state =
    case state.open of
        ( Open, hdr ) ->
            if header == hdr then
                div [ class "dropdown-list" ] <|
                    List.map
                        (\( item, header ) ->
                            label []
                                [ input [ type_ "checkbox", checked <| not (List.member ( item, header ) state.uncheckedEntries), onClick (FilterChecked ( item, header )) ] []
                                , text item
                                ]
                        )
                        state.dropdownSelection
            else
                text ""

        ( Closed, _ ) ->
            text ""


sortedEntries : State -> List Entry -> List Entry
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


main : Program Never State Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
