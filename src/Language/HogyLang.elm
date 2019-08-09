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
      , ( ReadInput, "ﾎｷﾞｨ…ﾎｷﾞｨ！" )
      , ( PrintOutput, "ﾎｷﾞｨ！ﾎｷﾞｨ…" )
      ]
    , "Hogy Lang"
    )
