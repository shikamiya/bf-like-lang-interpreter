module Language.Ook exposing (table)

import BFTypes exposing (BFTokenKind(..), BFTokenTable)


table : BFTokenTable
table =
    ( [ ( IncreasePointer, "Ook. Ook?" )
      , ( DecreasePointer, "Ook? Ook." )
      , ( IncreaseValue, "Ook. Ook." )
      , ( DecreaseValue, "Ook! Ook!" )
      , ( ReadInput, "Ook. Ook!" )
      , ( PrintOutput, "Ook! Ook." )
      , ( LoopStart, "Ook! Ook?" )
      , ( LoopEnd, "Ook? Ook!" )
      ]
    , "Ook!"
    )
