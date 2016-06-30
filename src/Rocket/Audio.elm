port module Rocket.Audio exposing (..)

{-| First String is for the id of the audio tag, second for the action
   actions can be: play, pause, reset
-}


port audio : (String, String) -> Cmd msg
