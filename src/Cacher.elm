port module Cacher exposing (cache)

import Json.Encode as E


port cache : E.Value -> Cmd msg
