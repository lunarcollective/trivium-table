module Trivium exposing (..)

import Html exposing (Html, text, div, input, label)
import Html.Events exposing (onClick, onInput, onBlur)
import Html.Attributes exposing (class, value, type_, checked, placeholder)


type Dropdown
    = Open
    | Closed


type Dir
    = Asc
    | Desc


type alias State a =
    { id : String
    , query : String
    , entries : List a
    , include : a -> Bool
    , isSelected : a -> Bool
    , isChecked : a -> Bool
    , checked : a -> Bool -> a
    , select : a -> String -> a
    , selected : List a
    , open : Bool
    , dropdownSelection : List a
    , uncheckedEntries : List a
    }


init : ( State a, Cmd msg )
init =
    ( State
        ""
        ""
        []
        (\_ -> False)
        (\_ -> True)
        (\_ -> True)
        (\e _ -> e)
        (\e _ -> e)
        []
        False
        ([])
        ([])
    , Cmd.none
    )


initWith : List a -> (a -> Bool) -> (a -> Bool) -> (a -> Bool) -> (a -> Bool -> a) -> (a -> String -> a) -> State a
initWith entries include isSelected isChecked e s =
    State "" "" entries include isSelected isChecked e s [] False ([]) ([])


type Msg b
    = NoOp
    | UpdateQuery String
    | Dropdown String
    | FilterChecked b
    | AlphaSort String
    | Select b
    | Deselect b
    | ToggleDropdown


update : Msg a -> State a -> ( State a, Cmd (Msg a) )
update msg state =
    case msg of
        UpdateQuery query ->
            ( { state
                | query = query
                , entries = List.map (\entry -> state.select entry query) state.entries
              }
            , Cmd.none
            )

        Dropdown header ->
            ( { state
                | dropdownSelection = state.entries
              }
            , Cmd.none
            )

        FilterChecked e ->
            -- let
            -- filterSet =
            --     case ( item, header ) of
            --         ( "All", header ) ->
            --             if List.member ( "All", header ) state.uncheckedEntries then
            --                 List.filter
            --                     (\( _, hdr ) -> hdr /= header)
            --                     state.uncheckedEntries
            --             else
            --                 ( "All", header ) :: state.uncheckedEntries ++ List.map (\entry -> ( getEntryString header entry, header )) state.entries
            --
            --         ( item, header ) ->
            --             if List.member ( item, header ) state.uncheckedEntries then
            --                 List.filter (\( itm, hdr ) -> ( itm, hdr ) /= ( item, header )) state.uncheckedEntries
            --             else
            --                 ( item, header ) :: state.uncheckedEntries
            -- in
            ( { state
                | uncheckedEntries = state.entries
                , entries =
                    List.map
                        (\entry ->
                            if entry == e then
                                state.checked entry True
                            else
                                entry
                        )
                        state.entries
              }
            , Cmd.none
            )

        Select entry ->
            ( { state | selected = insert entry state.selected }, Cmd.none )

        Deselect entry ->
            ( { state | selected = remove entry state.selected }, Cmd.none )

        ToggleDropdown ->
            ( { state | open = not state.open }, Cmd.none )

        _ ->
            ( state, Cmd.none )


insert entry selected =
    entry :: selected


remove : a -> List a -> List a
remove entry selected =
    List.filter (\e -> e /= entry) selected


flipDir : Dir -> Dir
flipDir dir =
    case dir of
        Asc ->
            Desc

        Desc ->
            Asc


getEntryString h e =
    ""


filterUncheckedEntries : (a -> Bool -> a) -> List a -> a -> a
filterUncheckedEntries checked filterSet entry =
    let
        nameInSet =
            True

        -- List.member ( entry.name, Name ) filterSet
        titleInSet =
            True

        -- List.member ( entry.title, Title ) filterSet
        locationInSet =
            True

        -- List.member ( entry.location, Location ) filterSet
    in
        if nameInSet || titleInSet || locationInSet then
            checked entry True
        else
            checked entry False


filterEntries selected query entry =
    let
        queryInName =
            True

        -- String.contains (String.toLower query) (String.toLower entry.name)
        queryInTitle =
            True

        -- String.contains (String.toLower query) (String.toLower entry.title)
        queryInLocation =
            True

        -- String.contains (String.toLower query) (String.toLower entry.location)
    in
        if queryInName || queryInTitle || queryInLocation then
            selected entry True
        else
            selected entry False


headerToEntryField sortType state entry =
    ( "FOO", sortType )



-- case sortType of
--     Name ->
--         ( entry.name, sortType )
--
--     Title ->
--         ( entry.title, sortType )
--
--     Location ->
--         ( entry.location, sortType )
-- view : State a -> Html (Msg a)


viewFilter selection state =
    if state.open then
        div [ class "dropdown-list" ] <|
            [ div [ class "button", onClick ToggleDropdown ] [ text "X" ]
            , input
                [ type_ "text"
                , onInput UpdateQuery
                , value state.query
                ]
                []
            ]
                ++ List.map
                    (\entry ->
                        if state.isSelected entry then
                            label []
                                [ input
                                    [ type_ "checkbox"
                                    , checked <| state.include entry
                                    , onClick (FilterChecked entry)
                                    ]
                                    []
                                , text <| selection entry
                                ]
                        else
                            div [] []
                    )
                    state.entries
    else
        div [ class "button", onClick ToggleDropdown ] [ text "+" ]


view state =
    div [ class "body__container" ]
        [ div [ class "input__container" ] []
        , div [ class "table__container" ]
            [ div [ class "table" ]
                [ div [ class "table__row headers" ] <| tableHeaders state
                , div [] <| showEntries state
                ]
            ]
        ]


showEntries state =
    let
        filteredEntries =
            -- state.entries
            List.filter state.isChecked state.entries
    in
        case filteredEntries of
            [] ->
                [ div [] [ text "No Matches" ] ]

            _ ->
                [ div [] <| tableBody filteredEntries state.selected ]


tableHeaders state =
    div [ class "table__entry--block flex" ] [ text "" ]
        :: List.map
            (\( action, string ) ->
                div [ class "table__entry--block flex" ]
                    [ div [ class "table__entry--text sortable", onClick (AlphaSort string) ] [ text string ]
                    , div [ class "drop-down-and-button" ]
                        [ div [ class "drop-down", onClick (Dropdown string) ]
                            [ text "+" ]
                        ]
                    ]
            )
            [--TODO: FIND HEADERS
            ]


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



--
-- sortedEntries : State a -> List { a | name : String, title : String, location : String } -> List { a | name : String, title : String, location : String }
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
