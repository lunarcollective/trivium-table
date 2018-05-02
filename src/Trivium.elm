module Trivium exposing (..)

import Html exposing (Html, text, div, input, label)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (class, value, type_, checked, placeholder)


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


type alias State a =
    { id : String
    , query : String
    , entries : List a
    , include : a -> Bool
    , selected : List a
    , open : ( Dropdown, Header )
    , dropdownSelection : List ( String, Header )
    , uncheckedEntries : List ( String, Header )
    , sortType : SortType
    }


init : ( State a, Cmd msg )
init =
    ( State
        ""
        ""
        []
        (\_ -> False)
        []
        ( Closed, Name )
        ([])
        ([])
        (SortType Asc Name)
    , Cmd.none
    )


initWith : List a -> (a -> Bool) -> State a
initWith entries include =
    State "" "" entries include [] ( Closed, Name ) ([]) ([]) (SortType Asc Name)


type Msg b
    = NoOp
    | UpdateQuery String
    | Dropdown String
    | FilterChecked ( String, Header )
    | AlphaSort String
    | Select b
    | Deselect b


update : Msg a -> State a -> ( State a, Cmd (Msg a) )
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


insert entry selected =
    entry :: selected


remove : a -> List a -> List a
remove entry selected =
    List.filter (\e -> e /= entry) selected


toggleFilter : String -> State a -> ( Dropdown, Header )
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


setDirection : Header -> State a -> Dir
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


getEntryString h e =
    ""



-- getEntryString : Header -> { a | name : String, title : String, location : String } -> String
-- getEntryString header entry =
--     case header of
--         Name ->
--             entry.name
--
--         Title ->
--             entry.title
--
--         Location ->
--             entry.location


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


filterEntries query entry =
    entry



-- filterEntries : String -> { a | selectionType : SelectionType, name : String, title : String, location : String } -> { a | selectionType : SelectionType, name : String, title : String, location : String }
-- filterEntries query entry =
--     let
--         queryInName =
--             String.contains (String.toLower query) (String.toLower entry.name)
--
--         queryInTitle =
--             String.contains (String.toLower query) (String.toLower entry.title)
--
--         queryInLocation =
--             String.contains (String.toLower query) (String.toLower entry.location)
--     in
--         if queryInName || queryInTitle || queryInLocation then
--             { entry | selectionType = Include }
--         else
--             { entry | selectionType = Exclude }


headerToEntryField sortType state entry =
    ( "", sortType )



-- headerToEntryField : Header -> State a -> { a | name : String, title : String, location : String } -> ( String, Header )
-- headerToEntryField sortType state entry =
--     case sortType of
--         Name ->
--             ( entry.name, sortType )
--
--         Title ->
--             ( entry.title, sortType )
--
--         Location ->
--             ( entry.location, sortType )
--
--
-- view : State a -> Html (Msg a)


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



-- showEntries : State a -> List (Html (Msg a))


showEntries state =
    let
        filteredEntries =
            List.filter state.include state.entries |> sortedEntries state
    in
        case filteredEntries of
            [] ->
                [ div [] [ text "No Matches" ] ]

            _ ->
                [ div [] <| tableBody filteredEntries state.selected ]



-- tableHeaders : State { a | name : String, title : String, location : String } -> List (Html Msg)


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



-- tableBody : List { a | name : String, title : String, location : String } -> List { a | name : String, title : String, location : String } -> List (Html (Msg { a | name : String, title : String, location : String }))


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



--


checkbox : List a -> a -> Html (Msg a)
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


showDropdown : Header -> State a -> Html (Msg a)
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



--
-- sortedEntries : State a -> List { a | name : String, title : String, location : String } -> List { a | name : String, title : String, location : String }


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



--
---- PROGRAM ----
-- main : Program Never (State a) (Msg a)


main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
