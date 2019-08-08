module Language.HogyLang exposing (table)

import BFTypes exposing (BFTokenKind(..), BFTokenTable)


table : BFTokenTable
table =
    ( [ ( LoopStart, "ﾎｷﾞｨ！ﾎｷﾞｨ〜" )
      , ( LoopEnd, "ﾎｷﾞｨ〜ﾎｷﾞｨ！" )
      , ( IncreaseValue, "ﾎｷﾞｨ…ﾎｷﾞｨ…" )
      , ( DecreaseValue, "ﾎｷﾞｨ！ﾎｷﾞｨ！" )
      , ( IncreasePointer, "ﾎｷﾞｨ…ﾎｷﾞｨ〜" )
      , ( DecreasePointer, "ﾎｷﾞｨ〜ﾎｷﾞｨ…" )
      , ( FetchInput, "ﾎｷﾞｨ…ﾎｷﾞｨ！" )
      , ( PrintCharacter, "ﾎｷﾞｨ！ﾎｷﾞｨ…" )
      ]
    , "Hogy Lang"
    )
