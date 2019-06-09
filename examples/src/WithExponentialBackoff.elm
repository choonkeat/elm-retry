module ExponentialBackoff exposing (Msg(..), init, main)

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

        -- Perform our desired `rawTask` with retries
        cmd =
            Retry.retry (rawTask "exponential interval")
                -- Configure to retry for a maximum of 7 seconds
                -- starting with a 500ms delay and exponentially backing off
                -- until a max of 3 seconds delay between retries
                |> Retry.withMaxDuration 7000
                |> Retry.withExponentialBackoff { interval = 500, maxInterval = 3000 }
                |> (Retry.toTask >> Task.attempt OnResult)
    in
    ( 0, cmd )


update : Msg -> model -> ( model, Cmd Msg )
update msg model =
    let
        _ =
            Debug.log "msg" msg
    in
    ( model, Cmd.none )
