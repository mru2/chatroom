module Chatroom (..) where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import StartApp
import Effects exposing (Effects, Never)
import Task exposing (Task)
import Json.Encode exposing (encode, string)


app : StartApp.App Model
app =
  StartApp.start
    { init = init
    , update = update
    , view = view
    , inputs = [ incomingActions ]
    }


main : Signal Html
main =
  app.html


port tasks : Signal (Task Never ())
port tasks =
  app.tasks



-- MODEL


type alias CurrentInput =
  { author : String
  , content : String
  }


type alias Message =
  { author : String
  , content : String
  }


type alias Model =
  { messages : List Message
  , current_input : CurrentInput
  }


init : ( Model, Effects Action )
init =
  let
    model =
      { messages = []
      , current_input =
          { author = "anonymous"
          , content = "Write some shit here"
          }
      }
  in
    ( model, Effects.none )



-- UPDATE


type Action
  = NoOp
  | Post Message
  | UpdateAuthor String
  | UpdateContent String
  | SubmitMessage
  | SetMessages (List Message)
  | AddMessage Message


updateAuthor : String -> CurrentInput -> CurrentInput
updateAuthor author current_input =
  { current_input | author = author }


updateContent : String -> CurrentInput -> CurrentInput
updateContent content current_input =
  { current_input | content = content }



-- submitCurrentInput : CurrentInput -> Task Http.Error (List String)
-- submitCurrentInput current_input =
--   let
--     json =
--       Json.Encode.encode
--         0
--         (Json.Encode.object
--           [ ( "author", Json.Encode.string current_input.author )
--           , ( "content", Json.Encode.string current_input.content )
--           ]
--         )
--   in
--     Http.post
--       (Json.Decode.list Json.Decode.string)
--       "/room"
--       (Http.string json)


update : Action -> Model -> ( Model, Effects Action )
update action model =
  case action of
    NoOp ->
      ( model, Effects.none )

    Post newMessage ->
      let
        model =
          { model | messages = newMessage :: model.messages }
      in
        ( model, Effects.none )

    SetMessages messages ->
      let
        model =
          { model | messages = messages }
      in
        ( model, Effects.none )

    AddMessage message ->
      let
        messages =
          message :: model.messages

        model =
          { model | messages = messages }
      in
        ( model, Effects.none )

    UpdateAuthor author ->
      let
        updateInput =
          updateAuthor author

        model =
          { model | current_input = updateInput (model.current_input) }
      in
        ( model, Effects.none )

    UpdateContent content ->
      let
        updateInput =
          updateContent (content)

        model =
          { model | current_input = updateInput (model.current_input) }
      in
        ( model, Effects.none )

    SubmitMessage ->
      let
        input =
          model.current_input
      in
        let
          effects =
            sendToPostMessage input

          clearInput input =
            { input | content = "" }

          model =
            { model | current_input = clearInput (model.current_input) }
        in
          ( model, effects )



-- VIEW


messageLine : Message -> Html
messageLine message =
  p
    []
    [ strong
        []
        [ text ("[" ++ message.author ++ "]")
        ]
    , span
        []
        [ text (" " ++ message.content)
        ]
    ]


inputFooter : Signal.Address Action -> CurrentInput -> Html
inputFooter address current_input =
  div
    [ class "row" ]
    [ div
        [ class "col-sm-2" ]
        [ div
            [ class "input-group" ]
            [ span [ class "input-group-addon" ] [ text "@" ]
            , input
                [ id "username"
                , type' "text"
                , class "form-control"
                , placeholder "username"
                , value current_input.author
                , on "input" targetValue (Signal.message address << UpdateAuthor)
                ]
                []
            ]
        ]
    , div
        [ class "col-sm-9" ]
        [ input
            [ id "message-input"
            , class "form-control"
            , value current_input.content
            , on "input" targetValue (Signal.message address << UpdateContent)
            ]
            []
        ]
    , div
        [ class "col-sm-1" ]
        [ button
            [ class "btn btn-primary"
            , onClick address SubmitMessage
            ]
            [ text "Go" ]
        ]
    ]


view : Signal.Address Action -> Model -> Html
view address model =
  div
    [ id "chatroom" ]
    [ div
        [ id "messages", class "container-fluid" ]
        (List.map messageLine model.messages)
    , div
        [ id "footer" ]
        [ div
            [ class "container-fluid" ]
            [ (inputFooter address model.current_input) ]
        ]
    ]



-- SIGNALS


port setMessages : Signal (List Message)
port addMessage : Signal Message
postMessageMailbox : Signal.Mailbox String
postMessageMailbox =
  Signal.mailbox ""


port postMessage : Signal String
port postMessage =
  postMessageMailbox.signal


sendToPostMessage : CurrentInput -> Effects Action
sendToPostMessage current_input =
  -- Only string can be passed around
  let
    payload =
      Json.Encode.object
        [ ( "author", string current_input.author )
        , ( "content", string current_input.content )
        ]

    json =
      encode 0 payload
  in
    Signal.send postMessageMailbox.address json
      |> Task.map (\t -> NoOp)
      |> Effects.task


incomingActions : Signal Action
incomingActions =
  Signal.merge
    (Signal.map SetMessages setMessages)
    (Signal.map AddMessage addMessage)
