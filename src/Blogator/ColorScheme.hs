module Blogator.ColorScheme where

import qualified Data.Map as Map
import Skylighting (Style (..), TokenStyle (..))
import Skylighting.Types

color :: Int -> Maybe Color
color = toColor

cyan = color 0x46a6b2
green = color 0x7da869
gray = color 0x545862
bg = color 0x1e222a
fg = color 0xabb2bf
alt_fg = color 0x8b92a8
purple = color 0xb668cd
light_blue = color 0xabb2bf
blue = color 0x519fdf
orange = color 0xc18a56
error_red = color 0xF44747

onedarker :: Style
onedarker =
  Style
    { backgroundColor = bg
    , defaultColor = fg
    , lineNumberColor = color 0xaaaaaa
    , lineNumberBackgroundColor = Nothing
    , tokenStyles =
        Map.fromList
          [ (KeywordTok, defStyle{tokenColor = purple})
          , (CharTok, defStyle{tokenColor = green})
          , (StringTok, defStyle{tokenColor = green})
          , (CommentTok, defStyle{tokenColor = gray})
          , (OtherTok, defStyle{tokenColor = fg})
          , (AlertTok, defStyle{tokenColor = error_red})
          , (ErrorTok, defStyle{tokenColor = error_red})
          , (WarningTok, defStyle{tokenColor = error_red})
          , (ConstantTok, defStyle{tokenColor = orange})
          , (SpecialCharTok, defStyle{tokenColor = purple})
          , (DataTypeTok, defStyle{tokenColor = cyan})
          , (FunctionTok, defStyle{tokenColor = blue})
          , (VariableTok, defStyle{tokenColor = light_blue})
          , (OperatorTok, defStyle{tokenColor = alt_fg})
          , (AnnotationTok, defStyle{tokenColor = blue})
          , (AttributeTok, defStyle{tokenColor = cyan})
          , (DecValTok, defStyle{tokenColor = cyan})
          ]
    }
