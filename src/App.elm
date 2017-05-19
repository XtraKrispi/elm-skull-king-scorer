module App exposing (..)

import Html exposing (program, div, Html, h1, text, i, h2, nav, a)
import Html.Attributes exposing (type_, class, colspan)
import Html.Events exposing (onSubmit, onClick)
import Bootstrap.Grid as Grid
import Bootstrap.Table as Table
import Bootstrap.ListGroup as ListGroup
import Bootstrap.Form.Input as Input
import Bootstrap.Form as Form
import Bootstrap.Button as Button
import Bootstrap.Form.InputGroup as InputGroup
import FontAwesome.Web as Icon
import Dom
import Task


type alias Round =
    Int


type alias Bet =
    Int


type alias TricksWon =
    Int


type alias BonusPoints =
    Int


type alias Winners =
    List Player


type alias PlayerBetInfo =
    ( Player, Maybe Bet, Maybe TricksWon, Maybe BonusPoints )


type alias PlayerHistoryInfo =
    ( Player, Bet, TricksWon, BonusPoints )


type alias RoundHistory =
    List ( Round, List PlayerHistoryInfo )


type alias RoundBetInfo =
    ( Round, List PlayerBetInfo )


type GameState
    = SelectingPlayers (Maybe Player) (List Player)
    | PlayingGame (List Player) RoundBetInfo RoundHistory
    | GameOver (List Player) RoundHistory Winners


type alias Player =
    String


type alias Model =
    GameState


type Msg
    = AddingPlayer String
    | EditPlayer Int String
    | RemovePlayer Int
    | AddPlayer
    | StartGame
    | EnterBet Int String
    | EnterTricksWon Int String
    | EnterBonusPoints Int String
    | AdvanceToNextRound
    | EndGame
    | StartNewGame
    | NoOp


isJust : Maybe a -> Bool
isJust a =
    case a of
        Just a_ ->
            True

        Nothing ->
            False


isNothing : Maybe a -> Bool
isNothing =
    not << isJust


main : Program Never Model Msg
main =
    program
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


init : ( Model, Cmd Msg )
init =
    SelectingPlayers Nothing [] ! [ Cmd.none ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        newHistoryRecords round pbs =
            ( round
            , List.concatMap
                (\( player, bet, tricksWon, bonusPoints ) ->
                    case ( bet, tricksWon, bonusPoints ) of
                        ( Just bet_, Just tricksWon_, Nothing ) ->
                            [ ( player, bet_, tricksWon_, 0 ) ]

                        ( Just bet_, Just tricksWon_, Just bonusPoints_ ) ->
                            [ ( player, bet_, tricksWon_, bonusPoints_ ) ]

                        _ ->
                            []
                )
                pbs
            )
    in
        case ( model, msg ) of
            ( SelectingPlayers cp ps, RemovePlayer i ) ->
                let
                    players =
                        List.map (\( _, p ) -> p) << List.filter (\( i_, p ) -> i_ /= i) << List.indexedMap (,)
                in
                    SelectingPlayers cp (players ps) ! [ Cmd.none ]

            ( SelectingPlayers (Just p) ps, AddPlayer ) ->
                SelectingPlayers Nothing (ps ++ [ p ]) ! [ Dom.focus "new-player" |> Task.attempt (always NoOp) ]

            ( SelectingPlayers p ps, AddingPlayer s ) ->
                case s of
                    "" ->
                        SelectingPlayers Nothing ps ! [ Cmd.none ]

                    s_ ->
                        SelectingPlayers (Just s) ps ! [ Cmd.none ]

            ( SelectingPlayers p ps, EditPlayer i s ) ->
                case s of
                    "" ->
                        SelectingPlayers p ps ! [ Cmd.none ]

                    s_ ->
                        SelectingPlayers p
                            (List.indexedMap
                                (\i_ p_ ->
                                    if i == i_ then
                                        s_
                                    else
                                        p_
                                )
                                ps
                            )
                            ! [ Cmd.none ]

            ( SelectingPlayers _ ps, StartGame ) ->
                let
                    firstRound =
                        ( 1, List.map (\p -> ( p, Nothing, Nothing, Nothing )) ps )
                in
                    PlayingGame ps firstRound [] ! [ Cmd.none ]

            ( PlayingGame ps (( round, pbs ) as r) h, EnterBet i b ) ->
                let
                    newBets isValid b_ =
                        List.indexedMap
                            (\ind ( player, bet, tricksWon, bonusPoints ) ->
                                if ind == i then
                                    ( player
                                    , if isValid then
                                        Just b_
                                      else
                                        Nothing
                                    , tricksWon
                                    , bonusPoints
                                    )
                                else
                                    ( player, bet, tricksWon, bonusPoints )
                            )
                            pbs
                in
                    case String.toInt b of
                        Ok b_ ->
                            PlayingGame ps ( round, newBets True b_ ) h ! [ Cmd.none ]

                        Err _ ->
                            PlayingGame ps ( round, newBets False 0 ) h ! [ Cmd.none ]

            ( PlayingGame ps (( round, pbs ) as r) h, EnterTricksWon i t ) ->
                let
                    newBets isValid t_ =
                        List.indexedMap
                            (\ind ( player, bet, tricksWon, bonusPoints ) ->
                                if ind == i then
                                    ( player
                                    , bet
                                    , if isValid then
                                        Just t_
                                      else
                                        Nothing
                                    , bonusPoints
                                    )
                                else
                                    ( player, bet, tricksWon, bonusPoints )
                            )
                            pbs
                in
                    case String.toInt t of
                        Ok t_ ->
                            PlayingGame ps ( round, newBets True t_ ) h ! [ Cmd.none ]

                        Err _ ->
                            PlayingGame ps ( round, newBets False 0 ) h ! [ Cmd.none ]

            ( PlayingGame ps (( round, pbs ) as r) h, EnterBonusPoints i bp ) ->
                let
                    newBets isValid bp_ =
                        List.indexedMap
                            (\ind ( player, bet, tricksWon, bonusPoints ) ->
                                if ind == i then
                                    ( player
                                    , bet
                                    , tricksWon
                                    , if isValid then
                                        Just bp_
                                      else
                                        Nothing
                                    )
                                else
                                    ( player, bet, tricksWon, bonusPoints )
                            )
                            pbs
                in
                    case String.toInt bp of
                        Ok bp_ ->
                            PlayingGame ps ( round, newBets True bp_ ) h ! [ Cmd.none ]

                        Err _ ->
                            PlayingGame ps ( round, newBets False 0 ) h ! [ Cmd.none ]

            ( PlayingGame ps (( round, pbs ) as r) h, AdvanceToNextRound ) ->
                PlayingGame ps ( round + 1, List.map (\p -> ( p, Nothing, Nothing, Nothing )) ps ) (h ++ [ newHistoryRecords round pbs ]) ! [ Cmd.none ]

            ( PlayingGame ps ( round, pbs ) h, EndGame ) ->
                let
                    computeGameWinners =
                        List.indexedMap (,) ps
                            |> List.foldr
                                (\( i, p ) ( w, wps ) ->
                                    let
                                        score =
                                            calculateTotalScore Nothing h i
                                    in
                                        if isNothing w || score > Maybe.withDefault 0 w then
                                            ( Just score, [ p ] )
                                        else if score == Maybe.withDefault 0 w then
                                            ( Just score, wps ++ [ p ] )
                                        else
                                            ( w, wps )
                                )
                                ( Nothing, [] )
                            |> (\( _, wps ) -> wps)
                in
                    GameOver ps (h ++ [ newHistoryRecords round pbs ]) computeGameWinners ! [ Cmd.none ]

            ( GameOver _ _ _, StartNewGame ) ->
                init

            ( _, NoOp ) ->
                model ! [ Cmd.none ]

            ( _, _ ) ->
                model ! [ Cmd.none ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


calculateScore : Int -> Maybe Bet -> Maybe TricksWon -> Maybe BonusPoints -> Int
calculateScore r bet tricksWon bonusPoints =
    let
        betScore =
            case ( bet, tricksWon ) of
                ( Just b, Just t ) ->
                    case ( b, abs <| b - t ) of
                        ( 0, 0 ) ->
                            r * 10

                        ( 0, _ ) ->
                            r * -10

                        ( _, 0 ) ->
                            b * 20

                        ( _, d ) ->
                            d * -10

                _ ->
                    0
    in
        Maybe.withDefault 0 bonusPoints + betScore


playerBet : RoundBetInfo -> Int -> Maybe PlayerBetInfo
playerBet b i =
    case b of
        ( _, pbs ) ->
            case List.head <| List.filter (\( i_, _ ) -> i == i_) <| List.indexedMap (,) pbs of
                Just ( _, pb ) ->
                    Just pb

                Nothing ->
                    Nothing


calculateTotalScore : Maybe RoundBetInfo -> RoundHistory -> Int -> Int
calculateTotalScore mb h i =
    List.foldr (\( r, b_, t, bp ) total -> total + (calculateScore r b_ t bp)) 0 <|
        (List.map (\( _, r, ( _, b_, t, bp_ ) ) -> ( r, Just b_, Just t, Just bp_ )) <|
            List.filter (\( i_, _, _ ) -> i == i_) <|
                List.concatMap (\( r, results ) -> List.indexedMap (\i result -> ( i, r, result )) results) h
        )
            ++ (case mb of
                    Just (( currentRound, _ ) as b) ->
                        case playerBet b i of
                            Just ( _, pb, ptw, pbp ) ->
                                [ ( currentRound, pb, ptw, pbp ) ]

                            Nothing ->
                                []

                    Nothing ->
                        []
               )


roundTable : List Player -> Maybe RoundBetInfo -> RoundHistory -> Html Msg
roundTable ps mb h =
    let
        historyRow ( round, playerHistory ) =
            Table.tr []
                ((Table.td [] [ text (toString round) ])
                    :: (List.concatMap
                            (\( player, bet, tricksWon, bonusPoints ) ->
                                [ Table.td [] [ text <| toString bet ]
                                , Table.td [] [ text <| toString tricksWon ]
                                , Table.td [] [ text <| toString bonusPoints ]
                                , Table.td [] [ text <| toString <| calculateScore round (Just bet) (Just tricksWon) (Just bonusPoints) ]
                                ]
                            )
                            playerHistory
                       )
                )

        currentRow ( round, betInfo ) =
            Table.tr []
                ((Table.td [] [ text (toString round) ])
                    :: (List.concatMap
                            (\( i, player, bet, tricksWon, bonusPoints ) ->
                                [ Table.td [] [ Input.number [ Input.onInput <| EnterBet i ] ]
                                , Table.td []
                                    [ Input.number [ Input.disabled <| isNothing bet, Input.onInput <| EnterTricksWon i ]
                                    ]
                                , Table.td [] [ Input.number [ Input.disabled <| isNothing tricksWon, Input.onInput <| EnterBonusPoints i ] ]
                                , Table.td [] [ text <| toString <| calculateScore round bet tricksWon bonusPoints ]
                                ]
                            )
                        <|
                            List.indexedMap (\i ( player, bet, tricksWon, bonusPoints ) -> ( i, player, bet, tricksWon, bonusPoints )) betInfo
                       )
                )

        topHeaderRow players =
            Table.tr [] <| (Table.th [] [ text "Round" ]) :: List.concatMap (\p -> [ Table.th [ Table.cellAttr (colspan 3) ] [ text p ], Table.th [] [ text "Score" ] ]) players

        bottomHeaderRow players =
            Table.tr [] <|
                (Table.th [] [])
                    :: (List.concatMap
                            (\( i, _ ) ->
                                [ Table.th [] [ text "Bet" ]
                                , Table.th [] [ text "Tricks" ]
                                , Table.th [] [ text "Bonus Points" ]
                                , Table.th [] [ text <| toString <| calculateTotalScore mb h i ]
                                ]
                            )
                        <|
                            List.indexedMap (,) players
                       )
    in
        div []
            [ Table.table
                { options = []
                , thead =
                    Table.thead []
                        [ topHeaderRow ps
                        , bottomHeaderRow ps
                        ]
                , tbody =
                    Table.tbody [] <|
                        (List.map historyRow h)
                            ++ (case mb of
                                    Just b ->
                                        [ currentRow b ]

                                    Nothing ->
                                        []
                               )
                }
            ]


playingGameView : List Player -> RoundBetInfo -> RoundHistory -> Html Msg
playingGameView ps (( currentRound, _ ) as b) h =
    let
        isAllDataFilled ( round, betInfo ) =
            List.all
                (\( _, bet, tricksWon, _ ) ->
                    case ( bet, tricksWon ) of
                        ( Just _, Just _ ) ->
                            True

                        _ ->
                            False
                )
                betInfo

        isDataValid (( round, betInfo ) as r) =
            let
                combinedData =
                    List.foldr
                        (\( _, bet, tricksWon, _ ) ( total, indValid ) ->
                            case ( bet, tricksWon ) of
                                ( Just bet_, Just tricksWon_ ) ->
                                    ( total + tricksWon_, (tricksWon_ <= round) && indValid )

                                _ ->
                                    ( total, False )
                        )
                        ( 0, True )
                        betInfo

                doTricksAddUp =
                    case combinedData of
                        ( _, False ) ->
                            False

                        ( r, True ) ->
                            round == r
            in
                isAllDataFilled r && doTricksAddUp
    in
        div []
            [ roundTable ps (Just b) h
            , div []
                [ if currentRound < 10 then
                    Button.button
                        [ Button.primary
                        , Button.disabled <| (not << isDataValid) b
                        , Button.onClick AdvanceToNextRound
                        ]
                        [ text "Next Round" ]
                  else
                    Button.button
                        [ Button.primary
                        , Button.disabled <| (not << isDataValid) b
                        , Button.onClick EndGame
                        ]
                        [ text "End the Game"
                        ]
                ]
            ]


contentView : Model -> Html Msg
contentView model =
    case model of
        SelectingPlayers p ps ->
            let
                existingPlayerInputGroup i p =
                    InputGroup.config (InputGroup.text [ Input.defaultValue p, Input.onInput <| EditPlayer i ])
                        |> InputGroup.successors
                            [ InputGroup.button [ Button.danger, Button.onClick (RemovePlayer i), Button.attrs [ type_ "button" ] ] [ Icon.trash ] ]
                        |> InputGroup.view

                newPlayerInputGroup =
                    InputGroup.config (InputGroup.text [ Input.id "new-player", Input.onInput AddingPlayer, Input.disabled <| List.length ps >= 8 ])
                        |> InputGroup.successors
                            [ InputGroup.button [ Button.primary, Button.attrs [ type_ "submit" ], Button.disabled <| isNothing p || List.length ps >= 8 ] [ Icon.plus ] ]
                        |> InputGroup.view

                startButtonDisabled =
                    (List.length ps < 2) || (List.length ps == 8)
            in
                Form.form [ onSubmit AddPlayer ]
                    [ h2 [] [ text "Players:" ]
                    , div []
                        [ ListGroup.ul <|
                            (List.indexedMap (\i p -> ListGroup.li [] [ existingPlayerInputGroup i p ]) ps)
                                ++ [ ListGroup.li []
                                        [ newPlayerInputGroup
                                        ]
                                   ]
                        , div [] [ Button.button [ Button.success, Button.attrs [ type_ "button" ], Button.onClick StartGame, Button.disabled startButtonDisabled ] [ text "Start the Game!" ] ]
                        ]
                    ]

        PlayingGame ps roundBetInfo roundHistory ->
            div []
                [ div []
                    [ playingGameView ps roundBetInfo roundHistory ]
                ]

        GameOver ps roundHistory winners ->
            let
                gameWinnerText =
                    case winners of
                        [ p ] ->
                            p ++ " has won the game!"

                        ps ->
                            (List.foldr
                                (\p s ->
                                    if s == "" then
                                        p
                                    else
                                        s ++ ", " ++ p
                                )
                                ""
                                ps
                            )
                                ++ " have tied for the win!"
            in
                div []
                    [ h2 []
                        [ text gameWinnerText ]
                    , div []
                        [ roundTable ps Nothing roundHistory ]
                    , div []
                        [ Button.button [ Button.success, Button.onClick StartNewGame ] [ text "Start a new game!" ] ]
                    ]


view : Model -> Html Msg
view model =
    Grid.container []
        [ nav [ class "navbar fixed-top navbar-light bg-faded" ]
            [ a [ class "navbar-brand" ] [ text "Skull King Scorer" ]
            ]
        , h1 [] [ text "Skull King Scorer" ]
        , contentView model
        ]
