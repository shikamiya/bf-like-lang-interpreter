module Language.BrainFuck exposing (table)

import BFTypes exposing (BFTokenKind(..), BFTokenTable)


table : BFTokenTable
table =
    ( [ ( LoopStart, "[" )
      , ( LoopEnd, "]" )
      , ( IncreaseValue, "+" )
      , ( DecreaseValue, "-" )
      , ( IncreasePointer, ">" )
      , ( DecreasePointer, "<" )
      , ( FetchInput, "," )
      , ( PrintCharacter, "." )
      ]
    , "BrainFuck"
    )
