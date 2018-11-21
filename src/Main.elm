module Main exposing (main)

import Array exposing (Array)
import Browser
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


type alias Checklist =
    { title : String, items : List ChecklistItem }


type alias ChecklistItem =
    { checked : Bool, description : String }


myChecklists : Array Checklist
myChecklists =
    Array.fromList
        [ mchecklist "Work Daily Review"
            [ "Journal"
            , "Install updates"
            , "Sync (tasks, books, passwords)"
            , "Refresh PGP keys"
            , "Fill hours in Odoo"
            ]
        , mchecklist "Work Weekly Review"
            [ "Clear desk"
            , "Empty Gmail Inbox"
            , "Empty Downloads"
            , "Empty Pictuces (screenshots)"
            ]
        , mchecklist "Weekly Review"
            [ "Collect loose papers and materials"
            , "Clear workspace"
            , "Empty physical inbox"
            , "Empty Email Inbox"
            , "Empty Inbox Folder"
            , "Empty Downloads"
            , "Empty Miniflux"
            ]
        ]


mchecklist : String -> List String -> Checklist
mchecklist title items =
    Checklist title <| List.map (ChecklistItem False) items


type alias Model =
    { currentChecklist : Maybe Int
    , checklists : Array Checklist
    }


getCurrentChecklist : Model -> Maybe Checklist
getCurrentChecklist m =
    m.currentChecklist |> Maybe.andThen (\i -> Array.get i m.checklists)


type Msg
    = SelectChecklist Int
    | Toggle Int


init : () -> ( Model, Cmd Msg )
init _ =
    ( { currentChecklist = Nothing, checklists = myChecklists }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg m =
    case msg of
        SelectChecklist i ->
            ( { m | currentChecklist = Just i }, Cmd.none )

        Toggle i ->
            m
                |> toggleItem i


toggleItem : Int -> Model -> ( Model, Cmd Msg )
toggleItem itemid model =
    case model.currentChecklist of
        Nothing ->
            ( model, Cmd.none )

        Just cid ->
            case Array.get cid model.checklists of
                Nothing ->
                    ( model, Cmd.none )

                Just cl ->
                    let
                        items =
                            List.indexedMap
                                (\i item ->
                                    if i == itemid then
                                        { item | checked = not item.checked }

                                    else
                                        item
                                )
                                cl.items

                        updatedCl =
                            { cl | items = items }
                    in
                    ( { model | checklists = Array.set cid updatedCl model.checklists }, Cmd.none )


updateItem mv =
    case mv of
        Nothing ->
            Just True

        Just v ->
            Just (not v)


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = \model -> { title = "Elm • TodoMVC", body = [ view model ] }
        , update = update
        , subscriptions = \_ -> Sub.none
        }


view : Model -> Html Msg
view m =
    case m.currentChecklist of
        Nothing ->
            viewChecklistMenu m.checklists

        Just i ->
            case Array.get i m.checklists of
                Nothing ->
                    div [] []

                Just checklist ->
                    viewChecklist checklist


viewChecklistMenu : Array Checklist -> Html Msg
viewChecklistMenu checklists =
    div []
        [ header [ class "tc ph4" ]
            [ h1 [ class "f3 f2-m f1-l fw2 black-90 mv3" ]
                [ text "Checklists" ]
            ]
        , ul [ class "pl0 list measure center" ] <| List.map viewChecklistLink <| Array.toIndexedList checklists
        ]


viewChecklist : Checklist -> Html Msg
viewChecklist checklist =
    div []
        [ header [ class "tc ph4" ]
            [ h1 [ class "f3 f2-m f1-l fw2 black-90 mv3" ]
                [ text checklist.title ]
            ]
        , section []
            [ ul [ class "pl0 list measure center" ] <| List.indexedMap viewChecklistItem checklist.items
            ]
        ]


viewChecklistItem : Int -> ChecklistItem -> Html Msg
viewChecklistItem id item =
    li [ class "pa3 lh-copy bb b--black-10" ]
        [ label []
            [ input
                [ class "mr2", Html.Attributes.checked item.checked, type_ "checkbox", onClick <| Toggle id ]
                []
            , text item.description
            ]
        ]


viewChecklistLink ( i, checklist ) =
    li [ class "pa3 bb b--black-10" ]
        [ a [ class " small-caps pa3 link", href "#", onClick (SelectChecklist i) ]
            [ text checklist.title ]
        ]
