module ConstantInterval exposing (Msg(..), init, main)

import Browser
import Retry
import Task exposing (Task)
import Time


{-| nothing much happening here, just logging `msg` on `update`
-}
main =
    Platform.worker
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        }


type Msg
    = OnResult (Result String String)


init : () -> ( Int, Cmd Msg )
init _ =
    let
        -- Given a task we want to run (in this case, it always `Task.fail name`)
        rawTask name =
            Time.now
                |> Task.map (Debug.log name)
                |> Task.andThen (\_ -> Task.fail name)

        -- Configure to retry for a maximum of 7 seconds
        -- retry 800ms after every failure
        config =
            [ Retry.maxDuration 7000
            , Retry.constantInterval 800
            ]

        -- Perform our desired `rawTask` with retries
        cmd =
            rawTask "constant interval"
                |> Retry.with config
                |> Task.attempt OnResult
    in
    ( 0, cmd )


update : Msg -> model -> ( model, Cmd Msg )
update msg model =
    let
        _ =
            Debug.log "msg" msg
    in
    ( model, Cmd.none )
