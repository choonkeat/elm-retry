module Retry exposing
    ( Policy(..), with
    , maxRetries, maxDuration, constantInterval, exponentialBackoff
    , TaskWithRetry, retry, toTask
    , withConstantInterval, withExponentialBackoff, withMaxDuration, withMaxRetries
    )

{-| Add retries to a task, based on a list of retry policies, until any one of
the policies fail too.

@docs Policy, with


## Common policies

@docs maxRetries, maxDuration, constantInterval, exponentialBackoff


# Pipelining API

@docs TaskWithRetry, retry, toTask


## Pipelined policies

@docs withConstantInterval, withExponentialBackoff, withMaxDuration, withMaxRetries

-}

import Process
import Random
import Task exposing (Task)
import Time exposing (now)


{-| A [`Policy`](#Policy) is attached with a function that will return another
[`Policy`](#Policy) as a [`Task`](https://package.elm-lang.org/packages/elm/core/latest/Task#Task) value.

The arguments of the function are

  - `Int` timestamp of when we first started `originalTask`, in milliseconds
  - `Policy x` the current policy; destructure to obtain the function to call
  - `x` last error from attempting `originalTask`

Refer to [`maxRetries`](#maxRetries) source code for a simple example.

-}
type Policy x
    = Policy (Int -> Policy x -> x -> Task x (Policy x))


{-| Given a list of error handling [`Policy`](#Policy) we can make our `originalTask`
retry on failure until any one of the [`Policy`](#Policy) fails.

    originalTask
        |> Retry.with
            [ Retry.maxDuration 7000
            , Retry.exponentialBackoff { interval = 500, maxInterval = 3000 }
            ]
        |> Task.attempt DidOriginalTask

-}
with : List (Policy x) -> Task x a -> Task x a
with errTasks originalTask =
    let
        onError startTime currPolicies err =
            currPolicies
                |> List.map (\((Policy nextPolicy) as cfg) -> nextPolicy startTime cfg err)
                |> Task.sequence
                |> Task.andThen (\nextPolicies -> Task.onError (onError startTime nextPolicies) originalTask)
    in
    Task.map Time.posixToMillis Time.now
        |> Task.andThen
            (\nowMillis -> Task.onError (onError nowMillis errTasks) originalTask)


{-| Stop retrying `originalTask` after a number of retries.

    Retry.with [ Retry.maxRetries 20 ] originalTask
        |> Task.attempt DidOriginalTask

NOTE: The code above does NOT sleep between retries; best to combine with
[`constantInterval`](#constantInterval) or [`exponentialBackoff`](#exponentialBackoff)

-}
maxRetries : Int -> Policy x
maxRetries int =
    let
        nextPolicy _ _ err =
            if int <= 0 then
                Task.fail err

            else
                Task.succeed (maxRetries (int - 1))
    in
    Policy nextPolicy


{-| Stop retrying `originalTask` after some number of milliseconds.

    Retry.with [ Retry.maxDuration 7000 ] originalTask
        |> Task.attempt DidOriginalTask

NOTE: The code above does NOT sleep between retries; best to combine with
[`constantInterval`](#constantInterval) or [`exponentialBackoff`](#exponentialBackoff)

-}
maxDuration : Int -> Policy x
maxDuration duration =
    let
        nextPolicy startTime sameTask err =
            Task.map Time.posixToMillis Time.now
                |> Task.andThen
                    (\now ->
                        if now - startTime >= duration then
                            Task.fail err

                        else
                            Task.succeed sameTask
                    )
    in
    Policy nextPolicy


{-| Sleep for the same number of milliseconds before every retry.

    Retry.with [ Retry.constantInterval 1000 ] originalTask
        |> Task.attempt DidOriginalTask

NOTE: The code above will keep retrying `originalTask`; best to combine with
[`maxRetries`](#maxRetries) or [`maxDuration`](#maxDuration)

-}
constantInterval : Float -> Policy x
constantInterval duration =
    let
        nextPolicy _ sameTask _ =
            Process.sleep duration
                |> Task.andThen (\_ -> Task.succeed sameTask)
    in
    Policy nextPolicy


{-| Sleep for an increasing number of milliseconds before every retry. Backoff
algorithim is based off [https://github.com/cenkalti/backoff](https://github.com/cenkalti/backoff/blob/4b4cebaf850ec58f1bb1fec5bdebdf8501c2bc3f/exponential.go#L144-L153)

    Retry.with [ Retry.exponentialBackoff { interval = 500, maxInterval = 3000 } ] originalTask
        |> Task.attempt DidOriginalTask

NOTE: The code above will keep retrying `originalTask`; best to combine with
[`maxRetries`](#maxRetries) or [`maxDuration`](#maxDuration)

-}
exponentialBackoff : { interval : Float, maxInterval : Float } -> Policy x
exponentialBackoff { interval, maxInterval } =
    let
        backoffWith seed currInterval =
            let
                ( calcInterval, nextSeed ) =
                    Random.step
                        (nextIntervalGenerator { randomizationFactor = 0.5, multiplier = 1.5, interval = currInterval })
                        seed

                nextPolicy _ _ err =
                    Process.sleep currInterval
                        |> Task.andThen (\_ -> Task.succeed (backoffWith nextSeed (min calcInterval maxInterval)))
            in
            Policy nextPolicy
    in
    backoffWith (Random.initialSeed 0) interval


nextIntervalGenerator : { randomizationFactor : Float, multiplier : Float, interval : Float } -> Random.Generator Float
nextIntervalGenerator { randomizationFactor, multiplier, interval } =
    let
        minInterval =
            interval * randomizationFactor

        maxInterval =
            interval * (1 + randomizationFactor)
    in
    Random.float 0 1
        |> Random.map (\randf -> multiplier * (minInterval + (randf * (maxInterval - minInterval + 1))))



-- Pipelining


{-| A Task with a list of [`Policy`](#Policy) attached.
-}
type TaskWithRetry x a
    = TaskWithRetry (List (Policy x)) (Task x a)


{-| Converts a [`Task`](https://package.elm-lang.org/packages/elm/core/latest/Task#Task)
into a [`TaskWithRetry`](#TaskWithRetry) to add error handling policies

    Retry.retry originalTask
        |> withMaxRetries 5
        |> withConstantInterval 500

-}
retry : Task x a -> TaskWithRetry x a
retry originalTask =
    TaskWithRetry [] originalTask


{-| Converts a [`TaskWithRetry`](#TaskWithRetry) back into a [`Task`](https://package.elm-lang.org/packages/elm/core/latest/Task#Task)
so we can perform our regular [`Task.attempt`](https://package.elm-lang.org/packages/elm/core/latest/Task#attempt)

    Retry.retry originalTask
        |> withMaxRetries 5
        |> withConstantInterval 500
        |> Retry.toTask
        |> Task.attempt DidOriginalTask

-}
toTask : TaskWithRetry x a -> Task x a
toTask (TaskWithRetry errTasks originalTask) =
    with errTasks originalTask


{-| adds [`maxRetries`](#maxRetries) policy in pipeline mode. Use in conjunction with [`retry`](#retry)
-}
withMaxRetries : Int -> TaskWithRetry x a -> TaskWithRetry x a
withMaxRetries int (TaskWithRetry errTasks originalTask) =
    TaskWithRetry (maxRetries int :: errTasks) originalTask


{-| adds [`maxDuration`](#maxDuration) policy in pipeline mode. Use in conjunction with [`retry`](#retry)
-}
withMaxDuration : Int -> TaskWithRetry x a -> TaskWithRetry x a
withMaxDuration duration (TaskWithRetry errTasks originalTask) =
    TaskWithRetry (maxDuration duration :: errTasks) originalTask


{-| adds [`maxConstantInterval`](#maxConstantInterval) policy in pipeline mode. Use in conjunction with [`retry`](#retry)
-}
withConstantInterval : Float -> TaskWithRetry x a -> TaskWithRetry x a
withConstantInterval duration (TaskWithRetry errTasks originalTask) =
    TaskWithRetry (constantInterval duration :: errTasks) originalTask


{-| adds [`exponentialBackoff`](#exponentialBackoff) policy in pipeline mode. Use in conjunction with [`retry`](#retry)
-}
withExponentialBackoff : { interval : Float, maxInterval : Float } -> TaskWithRetry x a -> TaskWithRetry x a
withExponentialBackoff expCfg (TaskWithRetry errTasks originalTask) =
    TaskWithRetry (exponentialBackoff expCfg :: errTasks) originalTask
