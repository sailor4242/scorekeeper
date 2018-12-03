module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


-- model


type alias Model =
    { players : List Player
    , name : String
    , playerId : Maybe Int
    , plays : List Play
    }


type alias Player =
    { id : Int
    , name : String
    , points : Int
    }


type alias Play =
    { id : Int
    , playerId : Int
    , name : String
    , points : Int
    }


init : Model
init =
    { players = []
    , name = ""
    , playerId = Nothing
    , plays = []
    }



-- update


type Msg
    = Edit Player
    | Score Player Int
    | Input String
    | Save
    | Cancel
    | DeletePlay Play


update : Msg -> Model -> Model
update msg model =
    case msg of
        Input name ->
            Debug.log "Input Updated Model"
                { model | name = name }

        Save -> 
            if String.isEmpty model.name then model 
            else case model.playerId of 
                Just id -> updatePlayer model id
                Nothing -> {model | players = savePlayer model, name =""}

        Cancel -> 
            {model | name = "", playerId = Nothing}

        Score player points -> 
            score model player points

        Edit player ->
            {model | name = player.name, playerId = Just player.id}
        DeletePlay play -> 
            deletePlay model play


deletePlay : Model -> Play -> Model
deletePlay model play =
    let
        newPlays =
            List.filter (\p -> p.id /= play.id) model.plays

        newPlayers =
            List.map
                (\player ->
                    if player.id == play.playerId then
                        { player | points = player.points - play.points }
                    else
                        player
                )
                model.players
    in
        { model | plays = newPlays, players = newPlayers }


score : Model -> Player -> Int -> Model
score model scorer points =
    let
        newPlayers =
            List.map
                (\player ->
                    if player.id == scorer.id then
                         { player | points = player.points + points }
                    else
                         player
                )
                model.players

        play = Play (List.length model.plays) scorer.id scorer.name points
    in
        { model | players = newPlayers, plays = play :: model.plays }


savePlayer : Model -> List Player
savePlayer model = 
    let
        player = Player (List.length model.players) model.name 0
    in
        player :: model.players


updatePlayer : Model -> Int -> Model
updatePlayer model id = 
    let
        newPlayers =
            List.map
                (\player ->
                    if player.id == id then
                        { player | name = model.name }
                    else
                        player
                )
                model.players

        newPlays =
            List.map
                (\play ->
                    if play.playerId == id then
                        { play | name = model.name }
                    else
                        play
                )
                model.plays
    in
        { model
            | players = newPlayers
            , plays = newPlays
            , name = ""
            , playerId = Nothing
        }


-- view

view : Model -> Html Msg
view model =
    div [ class "scoreboard" ]
        [ h1 [] [ text "Score Keeper" ]
        , playerSection model
        , playerForm model
        , playSection model
        ]


playerForm : Model -> Html Msg
playerForm model =
    Html.form [ onSubmit Save ]
        [ input
            [ type_ "text"
            , placeholder "Add/Edit Player..."
            , onInput Input
            , value model.name
            ]
            []
        , button [ type_ "submit" ] [ text "Save" ]
        , button [ type_ "button", onClick Cancel ] [ text "Cancel" ]
        ]


playerSection : Model -> Html Msg
playerSection model =
    div []
        [ playerListHeader
        , playerList model
        , pointTotal model
        ]

playerListHeader : Html Msg
playerListHeader =
    header []
        [ div [] [ text "Name" ]
        , div [] [ text "Points" ]
        ]


playerList : Model -> Html Msg
playerList model =
    model.players
        |> List.sortBy .name
        |> List.map playerView
        |> ul []


playerView : Player -> Html Msg
playerView player =
    li []
        [ i
            [ class "edit"
            , onClick (Edit player)
            ]
            []
        , div []
            [ text player.name ]
        , button
            [ type_ "button"
            , onClick (Score player 2)
            ]
            [ text "2pt" ]
        , button
            [ type_ "button"
            , onClick (Score player 3)
            ]
            [ text "3pt" ]
        , div []
            [ text (Debug.toString player.points) ]
        ]


pointTotal : Model -> Html Msg
pointTotal model =
    let
        total = 
            List.map .points model.plays
                |> List.sum
    in
        footer []
            [ div [] [ text "Total:" ]
            , div [] [ text (Debug.toString total) ]
            ]

playSection : Model -> Html Msg
playSection model =
    div []
        [ playListHeader
        , playList model
        ]


playListHeader : Html Msg
playListHeader =
    header []
        [ div [] [ text "Plays" ]
        , div [] [ text "Points" ]
        ]


playList : Model -> Html Msg
playList model =
    model.plays
        |> List.map playView
        |> ul []


playView : Play -> Html Msg
playView play =
    li []
        [ i
            [ class "remove"
            , onClick (DeletePlay play)
            ]
            []
        , div [] [ text play.name ]
        , div [] [ text (Debug.toString play.points) ]
        ]

main =
    Browser.sandbox { init = init, update = update, view = view }
