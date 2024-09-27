module Main exposing (..)

import Browser
import Html exposing (button, div, h1, h5, hr, input, li, text, ul)
import Html.Attributes exposing (attribute, checked, class, id, style, tabindex, type_, value, width)
import Html.Events exposing (onCheck, onClick, onInput)
import Html5.DragDrop as DragDrop
import Iso8601
import List.Extra
import Task
import Time


type alias Item =
    { description : String
    , createdTime : Maybe Time.Posix
    , checkedOff : Bool
    }


type BeforeIndex
    = BeforeIndex Int


type alias Model =
    { pendingDescription : String
    , items : List Item
    , dragDrop : DragDrop.Model Int BeforeIndex
    , hideCrossedOffItems : Bool
    }


type Msg
    = AddItem Item
    | UpdatePendingDescription String
    | RemoveItem Int
    | UpdateItem Int Item
    | AddItemTime Item Time.Posix
    | ToggleHideCrossedOffItems
    | DragDropMsg (DragDrop.Msg Int BeforeIndex)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { pendingDescription = ""
      , items = []
      , dragDrop = DragDrop.init
      , hideCrossedOffItems = False
      }
    , Cmd.none
    )


view : Model -> Html.Html Msg
view model =
    let
        showingItems =
            List.filter
                (if model.hideCrossedOffItems then
                    not << .checkedOff

                 else
                    always True
                )
                model.items

        dropId =
            DragDrop.getDropId model.dragDrop

        itemsHtml =
            List.Extra.interweave
                (List.Extra.initialize
                    (List.length showingItems + 1)
                    (viewDropZone dropId)
                )
                (List.indexedMap
                    viewItem
                    showingItems
                )
    in
    div []
        [ h1 [ Html.Attributes.class "text-8xl font-bold underline" ] [ text "TODO" ]
        , button
            [ Html.Attributes.type_ "button"
            , Html.Attributes.class "btn btn-primary"
            , onClick <|
                AddItem
                    { description = model.pendingDescription, createdTime = Nothing, checkedOff = False }
            ]
            [ text "+" ]
        , input [ onInput UpdatePendingDescription, value model.pendingDescription ] []
        , button [ class "ms-4", onClick ToggleHideCrossedOffItems ]
            [ text
                ((if model.hideCrossedOffItems then
                    "Show"

                  else
                    "Hide"
                 )
                    ++ " crossed off items"
                )
            ]
        , ul [ class "list-group" ] itemsHtml
        ]


viewItem : Int -> Item -> Html.Html Msg
viewItem i item =
    li
        (class "list-group-item" :: DragDrop.draggable DragDropMsg i)
        [ div []
            [ input
                [ type_ "checkbox"
                , class "form-check-input me-1"
                , checked item.checkedOff
                , onCheck (\checked -> UpdateItem i { item | checkedOff = checked })
                ]
                []
            , input
                ([ onInput <|
                    \s -> UpdateItem i { item | description = s }
                 , value <|
                    item.description
                 ]
                    ++ (if item.checkedOff then
                            [ style "color" "grey", style "text-decoration" "line-through" ]

                        else
                            []
                       )
                )
                []
            , button [ onClick <| RemoveItem i ] [ text "-" ]
            , button
                [ type_ "button"
                , attribute "data-bs-toggle" "modal"
                , attribute "data-bs-target" ("#" ++ itemModalId i)
                ]
                [ text "Edit" ]
            , viewItemDetailsModal i item
            ]
        ]


viewDropZone : Maybe BeforeIndex -> Int -> Html.Html Msg
viewDropZone dropId i =
    let
        hrStyle =
            case dropId of
                Nothing ->
                    []

                Just (BeforeIndex wouldDropIndex) ->
                    if wouldDropIndex == i then
                        [ class "border border-primary border-1 p-0", width 200 ]

                    else
                        []
    in
    div ([ style "align-items" "center", style "display" "flex", class "m-0" ] ++ DragDrop.droppable DragDropMsg (BeforeIndex i))
        [ hr hrStyle [] ]


itemModalId : Int -> String
itemModalId i =
    "item" ++ String.fromInt i ++ "Modal"


viewItemDetailsModal : Int -> Item -> Html.Html Msg
viewItemDetailsModal i item =
    div [ class "modal fade", id (itemModalId i), tabindex -1 ]
        [ div [ class "modal-dialog" ]
            [ div [ class "modal-content" ]
                [ div [ class "modal-header" ]
                    [ h5 [ class "modal-title" ] [ text ("Item Details for \"" ++ item.description ++ "\"") ]
                    , button [ type_ "button", class "btn-close", attribute "data-bs-dismiss" "modal", attribute "aria-label" "Close" ] []
                    ]
                , div [ class "modal-body" ]
                    [ text
                        ("Time created (UTC): "
                            ++ Maybe.withDefault "unknown"
                                (Maybe.map Iso8601.fromTime item.createdTime)
                        )
                    ]
                , div [ class "modal-footer" ]
                    [ button [ type_ "button", class "btn btn-secondary", attribute "data-bs-dismiss" "modal" ] [ text "Close" ]
                    , button [ type_ "button", class "btn btn-primary" ] [ text "Save changes" ]
                    ]
                ]
            ]
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddItem item ->
            ( { model | pendingDescription = "" }
            , Task.perform (AddItemTime item) Time.now
            )

        AddItemTime item time ->
            ( { model
                | items =
                    { item | createdTime = Just time }
                        :: model.items
              }
            , Cmd.none
            )

        UpdatePendingDescription newDescription ->
            ( { model | pendingDescription = newDescription }
            , Cmd.none
            )

        RemoveItem i ->
            ( { model | items = List.Extra.removeAt i model.items }
            , Cmd.none
            )

        UpdateItem i item ->
            ( { model | items = List.Extra.updateAt i (always item) model.items }
            , Cmd.none
            )

        DragDropMsg dragDropMsg ->
            let
                ( newDragDropModel, result ) =
                    DragDrop.update dragDropMsg model.dragDrop
            in
            ( { model
                | dragDrop = newDragDropModel
                , items =
                    case result of
                        Nothing ->
                            model.items

                        Just ( draggingIndex, droppingBeforeIndex, _ ) ->
                            moveFrom draggingIndex droppingBeforeIndex model.items
              }
            , Cmd.none
            )

        ToggleHideCrossedOffItems ->
            ( { model
                | hideCrossedOffItems = not model.hideCrossedOffItems
                , items = List.filter (not << .checkedOff) model.items ++ List.filter .checkedOff model.items
              }
            , Cmd.none
            )


{-| moves the element from the given original index to the new location before a given index

if either index is out of bounds, the original list is returned

moveFrom 0 (BeforeIndex 1) [1, 2] -> [1, 2]
moveFrom 0 (BeforeIndex 2) [1, 2, 3] -> [2, 1, 3]
moveFrom 0 (BeforeIndex 3) [1, 2, 3] -> [2, 3, 1]
moveFrom 2 (BeforeIndex 0) [1, 2, 3] -> [3, 1, 2]
moveFrom 2 (BeforeIndex 1) [1, 2, 3] -> [1, 3, 2]
moveFrom 2 (BeforeIndex 2) [1, 2, 3] -> [1, 2, 3]

-}
moveFrom : Int -> BeforeIndex -> List a -> List a
moveFrom originalIndex (BeforeIndex beforeIndex) xs =
    let
        movingItemMaybe =
            List.Extra.getAt originalIndex xs

        newBeforeIndex =
            if originalIndex < beforeIndex then
                beforeIndex - 1

            else
                beforeIndex

        moveItem movingItem =
            xs
                |> List.Extra.removeAt originalIndex
                |> insertBefore (BeforeIndex newBeforeIndex) movingItem
    in
    movingItemMaybe
        |> Maybe.map moveItem
        |> Maybe.withDefault xs


{-| inserts the given element before the given index
insertBefore (BeforeIndex 0) 1 [2, 3, 4] -> [1, 2, 3, 4]
insertBefore (BeforeIndex 3) 4 [1, 2, 3] -> [1, 2, 3, 4]
-}
insertBefore : BeforeIndex -> a -> List a -> List a
insertBefore (BeforeIndex i) x xs =
    List.take i xs ++ x :: List.drop i xs
